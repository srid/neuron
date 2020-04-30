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
    loadZettelkasten,

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
import Data.Dependent.Sum
import Data.Foldable (maximum)
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Data.Tree
import Development.Shake (Action)
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Error
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Eval (EvaluatedQuery (..), evaluateQueries)
import Neuron.Zettelkasten.Query.View (renderQueryLink)
import Neuron.Zettelkasten.Zettel
import Relude
import Text.MMark.Extension (Extension)
import Text.MMark.Extension.ReplaceLink (replaceLink)

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkasten :: [FilePath] -> Action (ZettelGraph, [(Zettel, Extension)])
loadZettelkasten files = do
  zettels <- mkZettelFromPath `mapM` files
  either (fail . show) pure $ mkZettelGraph zettels

-- | Build the Zettelkasten graph from a list of zettels
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
          (z,) <$> evaluateQueries zettels z
  let zettelsWithExtensions = fmap (replaceLink . fmap renderQueryLink) <$> zettelsWithQueryResults
      edges :: [(Maybe Connection, Zettel, Zettel)] = flip concatMap zettelsWithQueryResults $ \(z, qm) ->
        let conns :: [(Connection, Zettel)] = concatMap getConnections $ Map.elems qm
         in flip fmap conns $ \(cs, z2) -> (Just cs, z, z2)
  pure (G.mkGraphFrom zettels edges, zettelsWithExtensions)
  where
    -- TODO: Handle conflicts in edge monoid operation (same link but with
    -- different connection type), and consequently use a sensible type other
    -- than list.
    getConnections :: DSum Query EvaluatedQuery -> [(Connection, Zettel)]
    getConnections = \case
      Query_ZettelByID _ :=> EvaluatedQuery {..} ->
        [(evaluatedQueryConnection, evaluatedQueryResult)]
      Query_ZettelsByTag _ :=> EvaluatedQuery {..} ->
        (evaluatedQueryConnection,) <$> evaluatedQueryResult
      _ ->
        []

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
