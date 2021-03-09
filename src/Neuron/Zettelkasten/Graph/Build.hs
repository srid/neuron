{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph.Build
  ( buildZettelkasten,
  )
where

import Control.Monad.Writer.Strict (Writer, mapWriter, runWriterT, tell)
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Neuron.Markdown (lookupZettelMeta)
import Neuron.Plugin (PluginRegistry)
import qualified Neuron.Plugin as Plugin
import Neuron.Zettelkasten.Connection (ContextualConnection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Zettel (MissingZettel, Zettel, ZettelC, ZettelT (..), sansContent)
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
  PluginRegistry ->
  [ZettelC] ->
  Writer (Map ZettelID ZettelIssue) ZettelGraph
buildZettelkasten plugins parsedZettels = do
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
    mkZettelGraph plugins $ filter (not . unlisted) zs
  where
    unlisted =
      fromMaybe False . lookupZettelMeta @Bool "unlisted" . zettelMeta

-- | Build the Zettelkasten graph from a list of zettels
--
-- Report missing connections if any.
mkZettelGraph ::
  PluginRegistry ->
  [Zettel] ->
  Writer
    -- All zettels that have 1 or more missing wiki-links (indexed by containing zettel's ID)
    (Map ZettelID (Slug, NonEmpty MissingZettel))
    -- Zettel graph built from valid connections (pointing to existing zettels)
    ZettelGraph
mkZettelGraph plugins zettels = do
  let res :: [(Zettel, ([(ContextualConnection, Zettel)], [MissingZettel]))] =
        flip fmap zettels $ \z ->
          (z, runQueryConnections plugins zettels z)
  tell $
    Map.fromList $
      flip mapMaybe res $ \(z, nonEmpty . snd -> merrs) ->
        (zettelID z,) . (zettelSlug z,) <$> merrs
  pure $
    G.mkGraphFrom zettels $
      flip concatMap res $ \(z1, fst -> conns) ->
        edgeFromConnection z1 <$> conns

runQueryConnections ::
  PluginRegistry ->
  [Zettel] ->
  Zettel ->
  ( [(ContextualConnection, Zettel)],
    [MissingZettel]
  )
runQueryConnections plugins zettels z =
  flip runReader zettels $ do
    runWriterT $ do
      -- NOTE: Only the Links.hs plugin currently writes MissingZettel's.
      Plugin.graphConnections plugins z

edgeFromConnection :: Zettel -> (e, Zettel) -> (Maybe e, Zettel, Zettel)
edgeFromConnection z (c, z2) =
  (connectionMonoid c, z, z2)
  where
    -- Our connection monoid will never be Nothing (mempty); see the note in
    -- type `ZettelGraph`.
    connectionMonoid = Just
