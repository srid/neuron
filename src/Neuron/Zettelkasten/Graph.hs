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
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NEAM
import qualified Data.Map.Strict as Map
import Data.Tree (Forest, Tree (..))
import Development.Shake (Action)
import Neuron.Zettelkasten.ID
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
          zettelIDsFromMMark zettelContent <&> \(c, z2) -> ([c], zettelID, z2)
  -- TODO: Include all connections; show cf in "Connections" section
  -- But use Folgezettel only in index's topSort
  -- TODO: Handle conflicts. Link repeating but with different connection type
  pure $ LAM.edges $ filter (\(cs, _, _) -> not $ OrdinaryConnection `elem` cs) es

-- | Return the backlinks to the given zettel
backlinks :: ZettelID -> ZettelGraph -> [ZettelID]
backlinks zid =
  toList . LAM.preSet zid

clusters :: ZettelGraph -> [NonEmpty ZettelID]
clusters = fmap NEAM.vertexList1 . AM.vertexList . Algo.scc . LAM.skeleton

topSort :: ZettelGraph -> Either (NonEmpty ZettelID) [ZettelID]
topSort = Algo.topSort . LAM.skeleton

indexZettelID :: ZettelID
indexZettelID = ZettelID "OVERVIEW"

dfsForestFor :: Maybe (ZettelID -> Bool) -> ZettelID -> ZettelGraph -> Forest ZettelID
dfsForestFor f' fromZid =
  obviateRootUnlessForest fromZid . Algo.dfsForestFrom [fromZid] . LAM.skeleton . maybe id (LAM.induce . g) f'
  where
    -- Apply filter `f'` whilst unconditionally allowing the `fromZid` zettel.
    g f zid = or [zid == fromZid, f zid]

dfsForest :: ZettelID -> ZettelGraph -> Forest ZettelID
dfsForest =
  dfsForestFor Nothing

dfsForestBackwards :: ZettelID -> ZettelGraph -> Forest ZettelID
dfsForestBackwards fromZid =
  dfsForest fromZid . LAM.transpose

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
