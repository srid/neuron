{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Graph of zettels.
module Neuron.Zettelkasten.Graph where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Algo
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import qualified Data.Map.Strict as Map
import Data.Tree (Forest, Tree (..))
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link.Action (extractLinks, linkActionConnections)
import Neuron.Zettelkasten.Store (ZettelStore)
import Neuron.Zettelkasten.Type
import Relude

type ZettelGraph = LAM.AdjacencyMap [Connection] ZettelID

-- | Build the entire Zettel graph from the given list of note files.
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

-- | Return the backlinks to the given zettel
backlinks :: ZettelID -> ZettelGraph -> [ZettelID]
backlinks zid =
  toList . LAM.preSet zid

topSort :: ZettelGraph -> Either (NonEmpty ZettelID) [ZettelID]
topSort = Algo.topSort . LAM.skeleton

-- | Computer the dfsForest from either the given zettel or from all mother
-- vertices.
dfsForest :: Maybe ZettelID -> ZettelGraph -> Forest ZettelID
dfsForest fromZid g =
  Algo.dfsForestFrom startingZids $ LAM.skeleton g
  where
    startingZids = maybe motherVertices pure fromZid
    motherVertices =
      mapMaybe (\(v, es) -> if null es then Just v else Nothing)
        $ AM.adjacencyList
        $ LAM.skeleton
        $ LAM.transpose g

dfsForestBackwards :: ZettelID -> ZettelGraph -> Forest ZettelID
dfsForestBackwards fromZid =
  dfsForest (Just fromZid) . LAM.transpose

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
