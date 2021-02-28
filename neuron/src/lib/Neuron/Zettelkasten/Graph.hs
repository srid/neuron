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
    uplinkForest,
    backlinks,
    downlinks,
    backlinksMulti,
    categoryClusters,
    connectionCount,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Foldable (maximum)
import qualified Data.Graph.Labelled as G
import qualified Data.Set as Set
import Data.Tree (Forest, flatten)
import Neuron.Zettelkasten.Connection (Connection (..), ContextualConnection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Zettel (Zettel, ZettelT (zettelID))
import Relude

-- | TOD: move to Links
uplinkForest :: Zettel -> ZettelGraph -> Forest Zettel
uplinkForest z =
  G.obviateRootUnlessForest z
    . G.bfsForestBackwards z
    . folgezettelSubGraph

downlinks :: Zettel -> ZettelGraph -> [Zettel]
downlinks z =
  G.postSet z . folgezettelSubGraph

folgezettelSubGraph :: ZettelGraph -> ZettelGraph
folgezettelSubGraph =
  G.induceOnEdgeReplacing selectFolgezettelEdge

selectFolgezettelEdge :: (Maybe ContextualConnection, c, c) -> Maybe (Maybe ContextualConnection, c, c)
selectFolgezettelEdge (mconn, z1, z2) =
  case mconn of
    Just (Folgezettel, _) ->
      Just (mconn, z1, z2)
    Just (FolgezettelInverse, _) ->
      let ctx = mempty -- one $ B.Plain $ one $ B.Str "Folge Tag"
       in Just (Just (Folgezettel, ctx), z2, z1)
    _ -> Nothing

backlinks ::
  (Maybe Connection -> Bool) ->
  Zettel ->
  ZettelGraph ->
  [(ContextualConnection, Zettel)]
backlinks f z g =
  mapMaybe (\(e, v) -> (,v) <$> e) $
    G.preSetWithEdgeLabel (f . fmap fst) z g

-- | Like backlinks but for multiple zettels. More performant than calling
-- `backlinks` in a loop.
backlinksMulti ::
  (Functor f, Functor g) =>
  Connection ->
  f (g Zettel) ->
  ZettelGraph ->
  f (g (Zettel, [Zettel]))
backlinksMulti conn zs g =
  let f = G.preSetWithEdgeLabelMany ((== Just conn) . fmap fst) g
   in flip fmap zs $ \x ->
        flip fmap x $ \y ->
          (y, f y)

categoryClusters :: ZettelGraph -> [Forest Zettel]
categoryClusters g =
  let folgeGraph = folgezettelSubGraph g
      cs :: [[Zettel]] = sortMothers $ G.clusters folgeGraph
      cleanClusters = flip G.bfsForestFrom folgeGraph <$> cs
      clusteredZettels :: [Zettel] =
        (flatten `concatMap`) `concatMap` cleanClusters
      unclustered =
        Set.map zettelID $
          Set.fromList (getZettels g)
            `Set.difference` Set.fromList clusteredZettels
      uncleanCluster =
        G.dfsForest $
          G.induce (`Set.member` unclustered) g
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

-- | Return the connection if any between two zettels
--
-- If no connection exists, this returns Nothing.
getConnection :: Zettel -> Zettel -> ZettelGraph -> Maybe ContextualConnection
getConnection z1 z2 g =
  -- Use `join` so that empty edge monoid is treated as an abscence of edge
  -- (connection)
  join $ G.edgeLabel g z1 z2

connectionCount :: ZettelGraph -> Int
connectionCount = LAM.edgeCount . G.getGraph
