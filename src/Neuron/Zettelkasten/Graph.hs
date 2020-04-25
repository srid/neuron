{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph
  ( -- * Graph type
    ZettelGraph,

    -- * Construction
    loadZettelkasten,
  )
where

import Control.Monad.Except
import Data.Dependent.Sum
import Data.Graph.Labelled
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
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
      edges :: [([Connection], Zettel, Zettel)] = flip concatMap zettelsWithQueryResults $ \(z, qm) ->
        let conns :: [([Connection], Zettel)] = concatMap getConnections $ Map.elems qm
            connsFiltered = filter (connectionWhitelist . fst) conns
         in flip fmap connsFiltered $ \(cs, z2) -> (cs, z, z2)
  pure (mkGraphFrom zettels edges, zettelsWithExtensions)
  where
    -- TODO: Handle conflicts in edge monoid operation (same link but with
    -- different connection type), and consequently use a sensible type other
    -- than list.
    getConnections :: DSum Query EvaluatedQuery -> [([Connection], Zettel)]
    getConnections = \case
      Query_ZettelByID _ :=> EvaluatedQuery {..} ->
        [([evaluatedQueryConnection], evaluatedQueryResult)]
      Query_ZettelsByTag _ :=> EvaluatedQuery {..} ->
        ([evaluatedQueryConnection],) <$> evaluatedQueryResult
      _ ->
        []
    -- Exclude ordinary connection when building the graph
    --
    -- TODO: Build the graph with all connections, but induce a subgraph when
    -- building category forests. This way we can still show ordinary
    -- connetions in places (eg: a "backlinks" section) where they are
    -- relevant. See #34
    connectionWhitelist cs =
      OrdinaryConnection `notElem` cs
