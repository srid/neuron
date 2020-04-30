{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Algorithm where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Graph.Labelled.Type
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree (Forest, Tree (..))
import Relude

findVertex :: Ord (VertexID v) => VertexID v -> LabelledGraph v e -> Maybe v
findVertex v lg@(LabelledGraph g _) = do
  guard $ LAM.hasVertex v g
  pure $ getVertex lg v

getVertex :: (HasCallStack, Ord (VertexID a)) => LabelledGraph a e -> VertexID a -> a
getVertex (LabelledGraph _ vm) x =
  fromMaybe (error "Vertex not in map") $ Map.lookup x vm

getVertices :: LabelledGraph v e -> [v]
getVertices (LabelledGraph _ lm) =
  Map.elems lm

-- | Return the backlinks to the given vertex
preSet :: (Vertex v, Ord (VertexID v)) => v -> LabelledGraph v e -> [v]
preSet (vertexID -> zid) g =
  fmap (getVertex g) $ toList . LAM.preSet zid $ graph g

topSort :: (Vertex v, Ord (VertexID v)) => LabelledGraph v e -> Either (NonEmpty v) [v]
topSort g =
  bimap (fmap (getVertex g)) (fmap (getVertex g))
    $ Algo.topSort
    $ LAM.skeleton
    $ graph g

clusters :: (Vertex v, Ord (VertexID v)) => LabelledGraph v e -> [NonEmpty v]
clusters g =
  fmap (fmap $ getVertex g) $ mothers $ LAM.skeleton $ graph g

-- | Compute the dfsForest from the given vertices.
dfsForestFrom :: (Vertex v, Ord (VertexID v)) => [v] -> LabelledGraph v e -> Forest v
dfsForestFrom (fmap vertexID -> vs) g =
  fmap (fmap $ getVertex g) $ Algo.dfsForestFrom vs $ LAM.skeleton $ graph g

-- | Compute the dfsForest ending in the given vertex.
--
-- Return the forest flipped, such that the given vertex is the root.
dfsForestBackwards :: (Monoid e, Vertex v, Ord (VertexID v)) => v -> LabelledGraph v e -> Forest v
dfsForestBackwards fromV (LabelledGraph g' v') =
  dfsForestFrom [fromV] $ LabelledGraph (LAM.transpose g') v'

--------------------------
--- More general utilities
--------------------------

-- | Like `induce` but operates on edges instead of vertices
induceOnEdge :: Ord (VertexID v) => (e -> Bool) -> LabelledGraph v e -> LabelledGraph v e
induceOnEdge f (LabelledGraph g v) =
  LabelledGraph g' v
  where
    g' =
      let es = mapMaybe (\(e, a, b) -> if f e then Nothing else Just (a, b)) $ LAM.edgeList g
       in foldl' (\h (a, b) -> LAM.removeEdge a b h) g es

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
obviateRootUnlessForest :: (HasCallStack, Show a, Eq a) => a -> Forest a -> Forest a
obviateRootUnlessForest root = \case
  [Node v ts] ->
    if v == root
      then ts
      else error "Root mismatch"
  nodes ->
    nodes
