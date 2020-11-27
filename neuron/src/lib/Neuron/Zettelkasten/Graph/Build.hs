{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph.Build
  ( buildZettelkasten,
  )
where

import Control.Monad.Writer (runWriterT)
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Neuron.Reader.Type (ZettelFormat, ZettelReader)
import Neuron.Zettelkasten.Connection (Connection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Query.Error (QueryResultError)
import Neuron.Zettelkasten.Query.Eval (queryConnections)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelC,
    ZettelError (..),
    ZettelT (..),
    sansContent,
  )
import Neuron.Zettelkasten.Zettel.Parser (QueryExtractor, parseZettels)
import Relude
import Text.Pandoc.Definition (Block)

buildZettelkasten ::
  QueryExtractor ->
  [((ZettelFormat, ZettelReader), [(ZettelID, FilePath, Text)])] ->
  ( ZettelGraph,
    [ZettelC],
    Map ZettelID ZettelError
  )
buildZettelkasten queryExtractor fs =
  let zs = parseZettels queryExtractor fs
      zsC = sansContent <$> zs
      slugMap :: Map Slug (NonEmpty Zettel) =
        Map.fromListWith (<>) $
          zsC <&> (zettelSlug &&& one . id)
      zsCsplit =
        Map.toList slugMap <&> \(slug, zettels) ->
          case zettels of
            z :| [] ->
              -- Unique slug; accept this Zettel
              Left z
            _ ->
              -- Duplicate slugs
              Right (slug, zettels)

      (g, qErrs) = mkZettelGraph $ filter (not . zettelUnlisted) $ lefts zsCsplit
      errors =
        Map.unions
          [ fmap ZettelError_ParseError $
              Map.fromList $
                flip mapMaybe (lefts zs) $ \z ->
                  case zettelParseError z of
                    Just zerr -> Just (zettelID z, zerr)
                    _ -> Nothing,
            fmap ZettelError_QueryResultErrors qErrs,
            fmap ZettelError_AmbiguousSlugs $
              Map.fromList $
                concat $
                  rights zsCsplit <&> \(slug, zettels) ->
                    (,slug) . zettelID <$> toList zettels
          ]
   in (g, zs, errors)

-- | Build the Zettelkasten graph from a list of zettels
--
-- If there are any errors during parsing of queries (to determine connections),
-- return them as well.
mkZettelGraph ::
  [Zettel] ->
  ( ZettelGraph,
    Map ZettelID (Slug, NonEmpty QueryResultError)
  )
mkZettelGraph zettels =
  let res :: [(Zettel, ([((Connection, [Block]), Zettel)], [QueryResultError]))] =
        flip fmap zettels $ \z ->
          (z, runQueryConnections zettels z)
      g :: ZettelGraph = G.mkGraphFrom zettels $
        flip concatMap res $ \(z1, fst -> conns) ->
          edgeFromConnection z1 <$> conns
      errors = Map.fromList $
        flip mapMaybe res $ \(z, nonEmpty . snd -> merrs) ->
          (zettelID z,) . (zettelSlug z,) <$> merrs
   in (g, errors)

runQueryConnections :: [Zettel] -> Zettel -> ([((Connection, [Block]), Zettel)], [QueryResultError])
runQueryConnections zettels z =
  flip runReader zettels $ do
    runWriterT $ queryConnections z

edgeFromConnection :: Zettel -> (e, Zettel) -> (Maybe e, Zettel, Zettel)
edgeFromConnection z (c, z2) =
  (connectionMonoid c, z, z2)
  where
    -- Our connection monoid will never be Nothing (mempty); see the note in
    -- type `ZettelGraph`.
    connectionMonoid = Just
