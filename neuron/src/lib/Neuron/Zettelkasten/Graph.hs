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

    -- * Graph building
    mkZettelGraph,

    -- * Graph functions
    getZettels,
    getZettel,
    getConnection,
    topSort,
    frontlinkForest,
    backlinkForest,
    backlinks,
    clusters,
    categoryClusters,
  )
where

import Control.Monad.Writer (runWriterT)
import Data.Default
import Data.Foldable (maximum)
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Data.Tree
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error (QueryParseError)
import Neuron.Zettelkasten.Query.Eval (queryConnections)
import Neuron.Zettelkasten.Zettel
import Relude

-- | Build the Zettelkasten graph from a list of zettels
--
-- If there are any errors during parsing of queries (to determine connections),
-- return them as well.
mkZettelGraph ::
  [PandocZettel] ->
  ( ZettelGraph,
    Map ZettelID [QueryParseError]
  )
mkZettelGraph zettels =
  let res :: [(Zettel, ([(Maybe Connection, Zettel)], [QueryParseError]))] =
        flip runReader (fmap (fst . unPandocZettel) zettels) $ do
          for zettels $ \(PandocZettel (z, body)) -> fmap (z,) $ do
            runWriterT $ queryConnections body
      g :: ZettelGraph = G.mkGraphFrom (fst <$> res) $ flip concatMap res $ \(z1, fst -> conns) ->
        conns <&> \(c, z2) -> (connectionMonoid (fromMaybe Folgezettel c), z1, z2)
   in ( g,
        Map.fromList $ flip mapMaybe res $ \(z, (_conns, errs)) ->
          if null errs
            then Nothing
            else Just (zettelID z, errs)
      )
  where
    connectionMonoid = Just

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
  filter (not . branches) $ G.preSet z $ G.induceOnEdge (== Just conn) g
  where
    branches bz = G.hasEdge g z bz

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
