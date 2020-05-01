{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph
  ( -- * Graph type
    ZettelGraph,

    -- * Construction
    loadZettels,
    loadZettelkasten,
    loadZettelkastenFrom,

    -- * Graph functions
    topSort,
    frontlinkForest,
    backlinkForest,
    backlinks,
    clusters,
    categoryClusters,
    getZettels,
  )
where

import Control.Monad.Except
import Data.Foldable (maximum)
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Data.Tree
import Development.Shake (Action)
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Error
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.Query.Eval
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib
import Text.MMark.Extension (Extension)
import Text.MMark.Extension.ReplaceLink (replaceLink)

loadZettels :: Action [Zettel]
loadZettels =
  fmap (fmap fst . snd) loadZettelkasten

loadZettelkasten :: Action (ZettelGraph, [(Zettel, Extension)])
loadZettelkasten =
  loadZettelkastenFrom =<< Rib.forEvery ["*.md"] pure

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFrom :: [FilePath] -> Action (ZettelGraph, [(Zettel, Extension)])
loadZettelkastenFrom files = do
  zettels <- mkZettelFromPath `mapM` files
  either (fail . show) pure $ mkZettelGraph zettels

-- | Build the Zettelkasten graph from a list of zettels
--
-- Also return the markdown extension to use for each zettel.
mkZettelGraph ::
  forall m.
  MonadError NeuronError m =>
  [Zettel] ->
  m (ZettelGraph, [(Zettel, Extension)])
mkZettelGraph zettels = do
  zettelsWithQueryResults <-
    liftEither $ runExcept $ do
      for zettels $ \z ->
        withExcept (NeuronError_BadQuery (zettelID z)) $
          (z,) <$> evalZettelLinks zettels z
  zettelsWithExtensions <- for zettelsWithQueryResults $ \(z, resMap) -> liftEither $ runExcept $ do
    pure $ (z,) $ replaceLink $ snd `Map.map` resMap
  let edges :: [(Maybe Connection, Zettel, Zettel)] = flip concatMap zettelsWithQueryResults $ \(z, resMap) ->
        let conns :: [(Connection, Zettel)] = concatMap fst $ Map.elems resMap
         in conns <&> \(cs, z2) -> (Just cs, z, z2)
  pure (G.mkGraphFrom zettels edges, zettelsWithExtensions)

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
backlinks conn z =
  G.preSet z . G.induceOnEdge (== Just conn)

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
