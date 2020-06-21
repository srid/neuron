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
  )
where

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
    . G.dfsForestFrom [z]
    . G.induceOnEdge (== Just conn)

backlinkForest :: Connection -> Zettel -> ZettelGraph -> Forest Zettel
backlinkForest conn z =
  G.obviateRootUnlessForest z
    . G.dfsForestBackwards z
    . G.induceOnEdge (== Just conn)

backlinks :: Connection -> Zettel -> ZettelGraph -> [Zettel]
backlinks conn z g =
  -- FIXME: Calling this funtion in loop is inefficnet,
  -- due to induceOnEdge creating a whole new subgraph.
  filter (not . branches) $ G.preSet z $ G.induceOnEdge (== Just conn) g
  where
    branches bz = G.hasEdge g z bz

-- | Like backlinks but for multiple zettels. More performant.
-- TODO: Write performant version
backlinksMulti ::
  (Functor f, Functor g) =>
  Connection ->
  f (g Zettel) ->
  ZettelGraph ->
  f (g (Zettel, [Zettel]))
backlinksMulti conn zs g =
  flip fmap zs $ \x ->
    flip fmap x $ \y ->
      (y, backlinks conn y g)

categoryClusters :: ZettelGraph -> [Forest Zettel]
categoryClusters (categoryGraph -> g) =
  let cs :: [[Zettel]] = sortMothers $ clusters g
   in flip fmap cs $ \zs -> G.dfsForestFrom zs g
  where
    -- Sort clusters with newer mother zettels appearing first.
    sortMothers :: [NonEmpty Zettel] -> [[Zettel]]
    sortMothers = sortOn (Down . maximum) . fmap (sortOn Down . toList)

clusters :: ZettelGraph -> [NonEmpty Zettel]
clusters = G.clusters . categoryGraph

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
