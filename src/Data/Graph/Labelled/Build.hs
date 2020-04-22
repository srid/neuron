{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Build where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Graph.Labelled.Type
import Data.Traversable (for)
import Relude

-- Build a graph from a list objects that contains information about the
-- corresponding vertex as well as the outgoing edges.
mkGraphFrom ::
  forall m e v a.
  (Eq e, Monoid e, Ord v, Monad m) =>
  -- | List of objects corresponding to vertexes
  [a] ->
  -- | Make vertex from an object
  (a -> v) ->
  -- | Outgoing edges, and their vertex, for an object
  --
  -- Warning: This function may return vertices that do not belong to the graph.
  (a -> m [(e, v)]) ->
  -- | A function to filter relevant edges
  (e -> Bool) ->
  m (LabelledGraph v e)
mkGraphFrom xs vertexFor edgesFor edgeWhitelist = do
  let vertices = vertexFor <$> xs
  edges <-
    fmap concat $ for xs $ \x -> do
      es <- edgesFor x
      pure $ flip fmap es $ \(edge, v2) ->
        (edge, vertexFor x, v2)
  let edgesFinal = filter (\(e, _, _) -> edgeWhitelist e) edges
  pure $
    LAM.overlay
      (LAM.vertices vertices)
      (LAM.edges edgesFinal)
