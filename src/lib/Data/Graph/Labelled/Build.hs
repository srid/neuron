{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Build where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Graph.Labelled.Type (LabelledGraph (LabelledGraph), Vertex (..))
import qualified Data.Map.Strict as Map
import Relude

-- Build a graph from a list objects that contains information about the
-- corresponding vertex as well as the outgoing edges.
mkGraphFrom ::
  forall e v.
  (Eq e, Monoid e, Ord (VertexID v), Vertex v) =>
  -- | List of known vertices in the graph.
  [v] ->
  [(e, v, v)] ->
  LabelledGraph v e
mkGraphFrom xs es =
  let vertexList = vertexID <$> xs
      vertexMap = Map.fromList $ fmap (vertexID &&& id) xs
      edges = flip fmap es $ \(e, v1, v2) -> (e, vertexID v1, vertexID v2)
      graph =
        LAM.overlay
          (LAM.vertices vertexList)
          (LAM.edges edges)
   in LabelledGraph graph vertexMap
