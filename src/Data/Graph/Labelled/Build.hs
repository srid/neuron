{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Build where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Graph.Labelled.Type (LabelledGraph (LabelledGraph), Vertex (..))
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Relude

-- Build a graph from a list objects that contains information about the
-- corresponding vertex as well as the outgoing edges.
mkGraphFrom ::
  forall m e v.
  (Eq e, Monoid e, Ord (VertexID v), Vertex v, Monad m) =>
  -- | List of objects corresponding to vertexes
  [v] ->
  -- | Outgoing edges, and their vertex, for an object
  --
  -- Warning: The caller must ensure that this function returns vertices already
  -- in `[v]`.
  -- TODO: It is better to do this fmap'ing outside mkGraphFrom, and handle
  -- errors there.
  (v -> m [(e, VertexID v)]) ->
  -- | A function to filter relevant edges
  (e -> Bool) ->
  m (LabelledGraph v e)
mkGraphFrom xs edgesFor edgeWhitelist = do
  let vertexList = vertexID <$> xs
      vertexMap = Map.fromList $ fmap (vertexID &&& id) xs
  edges <-
    fmap concat $ for xs $ \x -> do
      es <- edgesFor x
      pure $ flip fmap es $ \(edge, v2) ->
        (edge, vertexID x, v2)
  let edgesFinal = filter (\(e, _, _) -> edgeWhitelist e) edges
      graph =
        LAM.overlay
          (LAM.vertices vertexList)
          (LAM.edges edgesFinal)
  pure $ LabelledGraph graph vertexMap