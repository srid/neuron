{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph.Build
  ( buildZettelkasten,
  )
where

import Control.Monad.Writer.Strict
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Neuron.Zettelkasten.Connection (ContextualConnection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Query.Eval (queryConnections)
import Neuron.Zettelkasten.Zettel
  ( MissingZettel,
    Zettel,
    ZettelC,
    ZettelT (..),
    sansContent,
  )
import Neuron.Zettelkasten.Zettel.Error (ZettelError (..), ZettelIssue (..))
import Relude

-- Build the zettelkasten graph from a list of zettels
--
-- Identify errors along the way:
--   - Parse errors (already in function input)
--   - Slug errors
--   - Query errors
-- The errors are gathered in the `snd` of the tuple.
buildZettelkasten ::
  [ZettelC] ->
  Writer (Map ZettelID ZettelIssue) ZettelGraph
buildZettelkasten parsedZettels = do
  -- Tell parse errors that are recorded in `zettelParseError` field.
  tell $
    Map.fromList $
      flip mapMaybe (lefts parsedZettels) $ \z -> do
        let zerr = snd $ zettelContent z
        pure (zettelID z, ZettelIssue_Error $ ZettelError_ParseError (zettelSlug z, zerr))
  -- Build a slug map to determined ambiguities in "slug" fields
  let slugMap :: Map Slug (NonEmpty Zettel) =
        Map.fromListWith (<>) $
          fmap sansContent parsedZettels <&> (zettelSlug &&& one)
  -- Pull out zettels with unambigous slugs
  zs <- fmap concat $
    forM (Map.toList slugMap) $ \case
      (_slug, z :| []) ->
        pure [z]
      (slug, zettels) -> do
        -- Duplicate slugs
        tell $
          Map.fromList $
            flip fmap (toList zettels) $ \z ->
              (zettelID z, ZettelIssue_Error $ ZettelError_AmbiguousSlug slug)
        pure []
  -- Build a graph from the final zettels list
  mapWriter (second $ fmap ZettelIssue_MissingLinks) $
    mkZettelGraph $ filter (not . zettelUnlisted) zs

-- | Build the Zettelkasten graph from a list of zettels
--
-- If there are any errors during parsing of queries (to determine connections),
-- return them as well.
mkZettelGraph ::
  [Zettel] ->
  Writer (Map ZettelID (Slug, NonEmpty MissingZettel)) ZettelGraph
mkZettelGraph zettels = do
  -- TODO: Also get connections via PluginData
  let res :: [(Zettel, ([(ContextualConnection, Zettel)], [MissingZettel]))] =
        flip fmap zettels $ \z ->
          (z, runQueryConnections zettels z)
  tell $
    Map.fromList $
      flip mapMaybe res $ \(z, nonEmpty . snd -> merrs) ->
        (zettelID z,) . (zettelSlug z,) <$> merrs
  pure $
    G.mkGraphFrom zettels $
      flip concatMap res $ \(z1, fst -> conns) ->
        edgeFromConnection z1 <$> conns

runQueryConnections :: [Zettel] -> Zettel -> ([(ContextualConnection, Zettel)], [MissingZettel])
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
