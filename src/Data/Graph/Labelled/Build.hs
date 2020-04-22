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
  -- | List of known vertices in the graph.
  [v] ->
  -- | Function returning the adjacents for each vertex.
  --
  -- Each adjacent vertex is expected to be in the original list.
  (v -> m [(e, v)]) ->
  -- | Edge whitelist function.
  (e -> Bool) ->
  m (LabelledGraph v e)
mkGraphFrom xs edgesFor edgeWhitelist = do
  let vertexList = vertexID <$> xs
      vertexMap = Map.fromList $ fmap (vertexID &&& id) xs
  edges <-
    fmap concat $ for xs $ \x -> do
      es <- edgesFor x
      pure $ flip fmap es $ \(edge, v2) ->
        (edge, vertexID x, vertexID v2)
  let edgesFinal = filter (\(e, _, _) -> edgeWhitelist e) edges
      graph =
        LAM.overlay
          (LAM.vertices vertexList)
          (LAM.edges edgesFinal)
  pure $ LabelledGraph graph vertexMap
