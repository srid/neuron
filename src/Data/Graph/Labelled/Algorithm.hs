{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Algorithm where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Graph.Labelled.Type
import qualified Data.Set as Set
import Data.Tree (Forest, Tree (..))
import Relude

-- | Return the backlinks to the given vertex
backlinks :: Ord v => v -> LabelledGraph v e -> [v]
backlinks zid =
  toList . LAM.preSet zid

topSort :: Ord v => LabelledGraph v e -> Either (NonEmpty v) [v]
topSort = Algo.topSort . LAM.skeleton

clusters :: Ord v => LabelledGraph v e -> [NonEmpty v]
clusters = mothers . LAM.skeleton

-- | Compute the dfsForest from the given vertices.
dfsForestFrom :: Ord v => [v] -> LabelledGraph v e -> Forest v
dfsForestFrom zids g =
  Algo.dfsForestFrom zids $ LAM.skeleton g

-- | Compute the dfsForest ending in the given vertex.
--
-- Return the forest flipped, such that the given vertex is the root.
dfsForestBackwards :: (Monoid e, Ord v) => v -> LabelledGraph v e -> Forest v
dfsForestBackwards fromZid =
  dfsForestFrom [fromZid] . LAM.transpose

--------------------------
--- More general utilities
--------------------------

-- | Get the clusters in a graph, as a list of the mother vertices in each
-- cluster.
mothers :: Ord a => AM.AdjacencyMap a -> [NonEmpty a]
mothers g =
  go [] $ motherVertices g
  where
    go acc = \case
      [] -> acc
      v : (Set.fromList -> vs) ->
        let reach = reachableUndirected v g
            covered = vs `Set.intersection` reach
            rest = vs `Set.difference` reach
         in go ((v :| Set.toList covered) : acc) (Set.toList rest)

-- | Get the vertexes reachable (regardless of direction) from the given vertex.
reachableUndirected :: Ord a => a -> AM.AdjacencyMap a -> Set a
reachableUndirected v =
  Set.fromList . Algo.reachable v . toUndirected
  where
    toUndirected g = AM.overlay g $ AM.transpose g

motherVertices :: Ord a => AM.AdjacencyMap a -> [a]
motherVertices =
  mapMaybe (\(v, es) -> if null es then Just v else Nothing)
    . AM.adjacencyList
    . AM.transpose

-- | If the input is a tree with the given root node, return its children (as
-- forest). Otherwise return the input as is.
obviateRootUnlessForest :: (HasCallStack, Show a, Eq a) => a -> [Tree a] -> [Tree a]
obviateRootUnlessForest root = \case
  [Node v ts] ->
    if v == root
      then ts
      else error "Root mismatch"
  nodes ->
    nodes
