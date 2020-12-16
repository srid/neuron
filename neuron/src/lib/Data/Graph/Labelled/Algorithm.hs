{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Algorithm where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Graph.Labelled.Type
  ( LabelledGraph (LabelledGraph, graph),
    Vertex (..),
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree (Forest, Tree (..))
import Relude

{-# INLINE getGraph #-}
getGraph :: LabelledGraph v e -> LAM.AdjacencyMap e (VertexID v)
getGraph (LabelledGraph g _) = g

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

hasEdge :: (Ord (VertexID v), Vertex v) => LabelledGraph v e -> v -> v -> Bool
hasEdge (LabelledGraph g _) x y =
  LAM.hasEdge (vertexID x) (vertexID y) g

edgeLabel :: (Monoid e, Ord (VertexID v), Vertex v) => LabelledGraph v e -> v -> v -> Maybe e
edgeLabel lg@(LabelledGraph g _) x y = do
  guard $ hasEdge lg x y
  pure $ LAM.edgeLabel (vertexID x) (vertexID y) g

-- | Return the backlinks to the given vertex
preSet :: (Vertex v, Ord (VertexID v)) => v -> LabelledGraph v e -> [v]
preSet (vertexID -> zid) g =
  fmap (getVertex g) $ toList . LAM.preSet zid $ graph g

-- | Return the preset of a vertex, considering only edges with the given label
--
-- WARNING: Dont' call this in a loop. For that, use preSetWithEdgeLabelMany
preSetWithEdgeLabel ::
  (Eq e, Monoid e, Vertex v, Ord (VertexID v)) =>
  (e -> Bool) ->
  v ->
  LabelledGraph v e ->
  [(e, v)]
preSetWithEdgeLabel f v g =
  let g' = LAM.transpose $ getGraph $ induceOnEdge f g
      ns = Map.toList $ Map.findWithDefault mempty (vertexID v) $ LAM.adjacencyMap g'
   in fmap (second (getVertex g) . swap) ns

-- | Optimized version of preSetWithEdgeLabel for multiple-input vertices.
preSetWithEdgeLabelMany ::
  (Eq e, Monoid e, Vertex v, Ord (VertexID v)) =>
  (e -> Bool) ->
  LabelledGraph v e ->
  (v -> [v])
preSetWithEdgeLabelMany f g =
  -- Compute the graph to search once, and then use it multiple times via the
  -- returned function.
  let g' = LAM.transpose $ graph $ induceOnEdge f g
   in \(vertexID -> v) -> fmap (getVertex g) $ toList $ LAM.postSet v g'

topSort :: (Vertex v, Ord (VertexID v)) => LabelledGraph v e -> Either (NonEmpty v) [v]
topSort g =
  bimap (fmap (getVertex g)) (fmap (getVertex g)) $
    Algo.topSort $
      LAM.skeleton $
        graph g

emap :: (Eq f, Monoid f) => (e -> f) -> LabelledGraph v e -> LabelledGraph v f
emap f (LabelledGraph g vs) =
  LabelledGraph (LAM.emap f g) vs

-- | Returns the clusters in an ayclic graph.
--
-- If the graph is one cluster and that is acyclic, this will return an empty list.
--
-- If any of the cluster is cyclic, that cluster will not be included.
clusters :: (Vertex v, Ord (VertexID v)) => LabelledGraph v e -> [NonEmpty v]
clusters g =
  fmap (fmap $ getVertex g) $ mothers $ LAM.skeleton $ graph g

-- | Compute the dfsForest from the given vertices.
dfsForest :: (Vertex v, Ord (VertexID v)) => LabelledGraph v e -> Forest v
dfsForest g =
  fmap (fmap $ getVertex g) $ Algo.dfsForest $ LAM.skeleton $ graph g

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

bfsForestBackwards :: (Monoid e, Vertex v, Ord (VertexID v)) => v -> LabelledGraph v e -> Forest v
bfsForestBackwards fromV (LabelledGraph g' v') =
  bfsForestFrom [fromV] $ LabelledGraph (LAM.transpose g') v'

bfsForestFrom :: (Vertex v, Ord (VertexID v)) => [v] -> LabelledGraph v e -> Forest v
bfsForestFrom (fmap vertexID -> vs) g =
  fmap (fmap $ getVertex g) $ Algo.bfsForest vs $ LAM.skeleton $ graph g

--------------------------
--- More general utilities
--------------------------

induce :: Ord (VertexID v) => (VertexID v -> Bool) -> LabelledGraph v e -> LabelledGraph v e
induce f (LabelledGraph g v) =
  LabelledGraph g' v
  where
    g' = LAM.induce f g

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
        let reach = reachableUndirected v
            covered = vs `Set.intersection` reach
            rest = vs `Set.difference` reach
         in go ((v :| Set.toList covered) : acc) (Set.toList rest)
    -- Vertices reachable from `v` regardless of direction.
    reachableUndirected v =
      Set.fromList $ Algo.reachable v gUndirected
    -- The undirected version of g
    gUndirected = AM.overlay g $ AM.transpose g

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
