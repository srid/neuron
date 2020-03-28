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
import Development.Shake (Action)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link.Action (extractLinks, linkActionConnections)
import Neuron.Zettelkasten.Store (ZettelStore)
import Neuron.Zettelkasten.Type
import Relude

type ZettelGraph = LAM.AdjacencyMap [Connection] ZettelID

-- | Build the entire Zettel graph from the given list of note files.
mkZettelGraph :: ZettelStore -> Action ZettelGraph
mkZettelGraph store = do
  -- Determine edges from links
  let es :: [([Connection], ZettelID, ZettelID)] =
        flip concatMap (Map.elems store) $ \Zettel {..} ->
          (linkActionConnections store `concatMap` extractLinks zettelContent)
            <&> \(c, z2) -> ([c], zettelID, z2)
  -- TODO: Handle conflicts. Link repeating but with different connection type
  pure $ LAM.edges $
    -- TODO: Include all connections; show cf in "Connections" section
    filter (\(cs, _, _) -> not $ OrdinaryConnection `elem` cs) es

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
