{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph
  ( -- * Graph type
    ZettelGraph,

    -- * Construction
    mkZettelGraph,

    -- * Algorithms
    backlinks,
    topSort,
    zettelClusters,
    dfsForestFrom,
    dfsForestBackwards,
    obviateRootUnlessForest,
  )
where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree (Forest, Tree (..))
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link.Action (extractLinks, linkActionConnections)
import Neuron.Zettelkasten.Store (ZettelStore)
import Neuron.Zettelkasten.Zettel
import Relude

-- | The Zettelkasten graph
type ZettelGraph = LAM.AdjacencyMap [Connection] ZettelID

-- | Build the Zettelkasten graph from the given list of note files.
mkZettelGraph :: ZettelStore -> ZettelGraph
mkZettelGraph store =
  mkGraphFrom (Map.elems store) zettelID zettelEdges connectionWhitelist
  where
    -- Exclude ordinary connection when building the graph
    --
    -- TODO: Build the graph with all connections, but induce a subgraph when
    -- building category forests. This way we can still show ordinary
    -- connetions in places (eg: a "backlinks" section) where they are
    -- relevant. See #34
    connectionWhitelist cs =
      not $ OrdinaryConnection `elem` cs
    -- Get the outgoing edges from this zettel
    --
    -- TODO: Handle conflicts in edge monoid operation (same link but with
    -- different connection type), and consequently use a sensible type other
    -- than list.
    zettelEdges :: Zettel -> [([Connection], ZettelID)]
    zettelEdges Zettel {..} =
      let outgoingLinks = linkActionConnections store `concatMap` extractLinks zettelContent
       in first pure <$> outgoingLinks

-- | Return the backlinks to the given zettel
backlinks :: ZettelID -> ZettelGraph -> [ZettelID]
backlinks zid =
  toList . LAM.preSet zid

topSort :: ZettelGraph -> Either (NonEmpty ZettelID) [ZettelID]
topSort = Algo.topSort . LAM.skeleton

zettelClusters :: ZettelGraph -> [NonEmpty ZettelID]
zettelClusters = mothers . LAM.skeleton

-- | Compute the dfsForest from the given zettels.
dfsForestFrom :: [ZettelID] -> ZettelGraph -> Forest ZettelID
dfsForestFrom zids g =
  Algo.dfsForestFrom zids $ LAM.skeleton g

-- | Compute the dfsForest ending in the given zettel.
--
-- Return the forest flipped, such that the given zettel is the root.
dfsForestBackwards :: ZettelID -> ZettelGraph -> Forest ZettelID
dfsForestBackwards fromZid =
  dfsForestFrom [fromZid] . LAM.transpose

-- -------------
-- Graph Helpers
-- -------------

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
obviateRootUnlessForest :: (Show a, Eq a) => a -> [Tree a] -> [Tree a]
obviateRootUnlessForest root = \case
  [Node v ts] ->
    if v == root
      then ts
      else error "Root mismatch"
  nodes ->
    nodes

-- Build a graph from a list objects that contains information about the
-- corresponding vertex as well as the outgoing edges.
mkGraphFrom ::
  (Eq e, Monoid e, Ord v) =>
  -- | List of objects corresponding to vertexes
  [a] ->
  -- | Make vertex from an object
  (a -> v) ->
  -- | Outgoing edges, and their vertex, for an object
  (a -> [(e, v)]) ->
  -- | A function to filter relevant edges
  (e -> Bool) ->
  LAM.AdjacencyMap e v
mkGraphFrom xs vertexFor edgesFor edgeWhitelist =
  let vertices =
        vertexFor <$> xs
      edges =
        flip concatMap xs $ \x ->
          edgesFor x
            <&> \(edge, v2) ->
              (edge, vertexFor x, v2)
   in LAM.overlay
        (LAM.vertices vertices)
        (LAM.edges $ filter (\(e, _, _) -> edgeWhitelist e) edges)
