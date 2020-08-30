{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph
  ( -- * Graph type
    ZettelGraph,

    -- * Graph functions
    getZettels,
    getZettel,
    getConnection,
    frontlinkForest,
    backlinkForest,
    backlinks,
    backlinksMulti,
    categoryClusters,
    connectionCount,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Foldable (maximum)
import qualified Data.Graph.Labelled as G
import qualified Data.Set as Set
import Data.Tree
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Relude

frontlinkForest :: Connection -> Zettel -> ZettelGraph -> Forest Zettel
frontlinkForest conn z =
  G.obviateRootUnlessForest z
    . G.bfsForestFrom [z]
    . G.induceOnEdge (== Just conn)

backlinkForest :: Connection -> Zettel -> ZettelGraph -> Forest Zettel
backlinkForest conn z =
  G.obviateRootUnlessForest z
    . G.bfsForestBackwards z
    . G.induceOnEdge (== Just conn)

backlinks ::
  (Maybe Connection -> Bool) ->
  Zettel ->
  ZettelGraph ->
  [(Connection, Zettel)]
backlinks f z g =
  mapMaybe (\(e, v) -> (,v) <$> e) $
    G.preSetWithEdgeLabel f z g

-- | Like backlinks but for multiple zettels. More performant than calling
-- `backlinks` in a loop.
backlinksMulti ::
  (Functor f, Functor g) =>
  Connection ->
  f (g Zettel) ->
  ZettelGraph ->
  f (g (Zettel, [Zettel]))
backlinksMulti conn zs g =
  let f = G.preSetWithEdgeLabelMany (Just conn) g
   in flip fmap zs $ \x ->
        flip fmap x $ \y ->
          (y, f y)

categoryClusters :: ZettelGraph -> [Forest Zettel]
categoryClusters (G.induceOnEdge (== Just Folgezettel) -> g) =
  let cs :: [[Zettel]] = sortMothers $ G.clusters g
      cleanClusters = flip G.bfsForestFrom g <$> cs
      clusteredZettels :: [Zettel] =
        (flatten `concatMap`) `concatMap` cleanClusters
      unclustered =
        Set.map zettelID $
          Set.fromList (getZettels g)
            `Set.difference` Set.fromList clusteredZettels
      uncleanCluster =
        G.dfsForest $
          G.induce (flip Set.member unclustered) g
   in cleanClusters
        <> if null uncleanCluster
          then mempty
          else pure uncleanCluster
  where
    -- Sort clusters with newer mother zettels appearing first.
    sortMothers :: [NonEmpty Zettel] -> [[Zettel]]
    sortMothers = sortOn (Down . maximum) . fmap (sortOn Down . toList)

getZettels :: ZettelGraph -> [Zettel]
getZettels = G.getVertices

getZettel :: ZettelID -> ZettelGraph -> Maybe Zettel
getZettel = G.findVertex

-- | If no connection exists, this returns Nothing.
getConnection :: Zettel -> Zettel -> ZettelGraph -> Maybe Connection
getConnection z1 z2 g = join $ G.edgeLabel g z1 z2

connectionCount :: ZettelGraph -> Int
connectionCount = LAM.edgeCount . G.getGraph
