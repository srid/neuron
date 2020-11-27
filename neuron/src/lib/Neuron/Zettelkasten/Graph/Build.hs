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
import Relude
import Text.Pandoc.Definition (Block)

-- Build the zettelkasten graph from a list of zettels
--
-- Identify errors along the way:
--   - Parse errors (already in function input)
--   - Slug errors
--   - Query errors
-- The errors are gathered in the `snd` of the tuple. This should probably be
-- refactored to use some Haskell idiom (Writer monad?).
buildZettelkasten ::
  [ZettelC] ->
  ( ZettelGraph,
    Map ZettelID ZettelError
  )
buildZettelkasten (fmap sansContent &&& lefts -> (zs', zsEParse)) =
  let slugMap :: Map Slug (NonEmpty Zettel) =
        Map.fromListWith (<>) $
          zs' <&> (zettelSlug &&& one . id)
      (zs, zsESlug) =
        (lefts &&& rights) $
          Map.toList slugMap <&> \(slug, zettels) ->
            case zettels of
              z :| [] ->
                -- Unique slug; accept this Zettel
                Left z
              _ ->
                -- Duplicate slugs
                Right (slug, zettels)
      (g, qErrs) = mkZettelGraph $ filter (not . zettelUnlisted) zs
      errors =
        Map.unions
          [ fmap ZettelError_ParseError $
              Map.fromList $
                flip mapMaybe zsEParse $ \z ->
                  case zettelParseError z of
                    Just zerr ->
                      Just (zettelID z, (zettelSlug z, zerr))
                    _ ->
                      Nothing,
            fmap ZettelError_QueryResultErrors qErrs,
            fmap ZettelError_AmbiguousSlug $
              Map.fromList $
                concat $
                  zsESlug <&> \(slug, zettels) ->
                    (,slug) . zettelID <$> toList zettels
          ]
   in (g, errors)

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
