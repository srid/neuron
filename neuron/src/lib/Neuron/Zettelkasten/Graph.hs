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
    topSort,
    frontlinkForest,
    backlinkForest,
    backlinks,
    backlinksMulti,
    clusters,
    categoryClusters,
    connectionCount,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Default
import Data.Foldable (maximum)
import qualified Data.Graph.Labelled as G
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

backlinks :: Connection -> Zettel -> ZettelGraph -> [Zettel]
backlinks conn z g =
  G.preSetWithEdgeLabel (Just conn) z g

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
categoryClusters (categoryGraph -> g) =
  let cs :: [[Zettel]] = sortMothers $ clusters g
   in flip fmap cs $ \zs -> G.bfsForestFrom zs g
  where
    -- Sort clusters with newer mother zettels appearing first.
    sortMothers :: [NonEmpty Zettel] -> [[Zettel]]
    sortMothers = sortOn (Down . maximum) . fmap (sortOn Down . toList)

clusters :: ZettelGraph -> [NonEmpty Zettel]
clusters g =
  case (G.clusters $ categoryGraph g) of
    [] ->
      maybe [] pure $ nonEmpty $ G.getVertices g
    cs ->
      cs

topSort :: ZettelGraph -> Either (NonEmpty Zettel) [Zettel]
topSort = G.topSort . categoryGraph

categoryGraph :: ZettelGraph -> ZettelGraph
categoryGraph = G.induceOnEdge (== Just Folgezettel)

getZettels :: ZettelGraph -> [Zettel]
getZettels = G.getVertices

getZettel :: ZettelID -> ZettelGraph -> Maybe Zettel
getZettel = G.findVertex

-- | If no connection exists, this returns Nothing.
getConnection :: Zettel -> Zettel -> ZettelGraph -> Maybe Connection
getConnection z1 z2 g = fmap (fromMaybe def) $ G.edgeLabel g z1 z2

connectionCount :: ZettelGraph -> Int
connectionCount = LAM.edgeCount . G.getGraph
