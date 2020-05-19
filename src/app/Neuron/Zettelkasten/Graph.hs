{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad.Except (MonadError, liftEither, runExceptT, withExceptT)
import Control.Monad.Writer (runWriterT)
import Data.Default
import Data.Foldable (maximum)
import qualified Data.Graph.Labelled as G
import Data.Traversable (for)
import Data.Tree
import Development.Shake
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Error
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Eval
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib
import System.FilePath

loadZettels :: Action [Zettel]
loadZettels =
  fmap getZettels loadZettelkasten

loadZettelkasten :: Action ZettelGraph
loadZettelkasten =
  loadZettelkastenFrom =<< Rib.forEvery ["*.md"] pure

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFrom :: [FilePath] -> Action ZettelGraph
loadZettelkastenFrom files = do
  notesDir <- Rib.ribInputDir
  zettels <- forM files $ \((notesDir </>) -> path) -> do
    s <- toText <$> readFile' path
    let zid = mkZettelID path
    case mkZettelFromMarkdown zid s snd of
      Left e -> fail $ toString e
      Right zettel -> pure zettel
  either (fail . show) pure $ mkZettelGraph zettels

-- | Build the Zettelkasten graph from a list of zettels
--
-- Also return the markdown extension to use for each zettel.
mkZettelGraph ::
  forall m.
  MonadError NeuronError m =>
  [Zettel] ->
  m ZettelGraph
mkZettelGraph zettels = do
  res :: [(Zettel, [(Maybe Connection, Zettel)])] <- liftEither =<< do
    flip runReaderT zettels $ runExceptT $ do
      for zettels $ \z -> withExceptT (NeuronError_BadQuery (zettelID z)) $ do
        runWriterT $ expandQueries z
  let g :: ZettelGraph = G.mkGraphFrom (fst <$> res) $ flip concatMap res $ \(z1, conns) ->
        conns <&> \(c, z2) -> (connectionMonoid (fromMaybe Folgezettel c), z1, z2)
  pure g
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
