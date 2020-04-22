{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph
  ( -- * Graph type
    ZettelGraph,

    -- * Construction
    loadZettelkasten,

    -- * Algorithm reports
    backlinks,
    topSort,
    clusters,
    dfsForestFrom,
    dfsForestBackwards,
    obviateRootUnlessForest,
  )
where

import Control.Monad.Except
import Data.Graph.Labelled.Algorithm
import Data.Graph.Labelled.Build
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Development.Shake (Action)
import Neuron.Zettelkasten.Error
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link (neuronLinkConnections, neuronLinkFromMarkdownLink)
import Neuron.Zettelkasten.Markdown (extractLinks)
import Neuron.Zettelkasten.Zettel
import Relude

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkasten :: [FilePath] -> Action ZettelGraph
loadZettelkasten files = do
  zettels <- mkZettelFromPath `mapM` files
  either (fail . toString) pure $ mkZettelGraph zettels

-- | Build the Zettelkasten graph from a list of zettels
mkZettelGraph :: forall m. MonadError Text m => [Zettel] -> m ZettelGraph
mkZettelGraph zettels =
  mkGraphFrom @m zettels zettelEdges connectionWhitelist
  where
    zettelsMap = Map.fromList $ zettels <&> zettelID &&& id
    -- Exclude ordinary connection when building the graph
    --
    -- TODO: Build the graph with all connections, but induce a subgraph when
    -- building category forests. This way we can still show ordinary
    -- connetions in places (eg: a "backlinks" section) where they are
    -- relevant. See #34
    connectionWhitelist cs =
      OrdinaryConnection `notElem` cs
    -- Get the outgoing edges from this zettel
    --
    -- TODO: Handle conflicts in edge monoid operation (same link but with
    -- different connection type), and consequently use a sensible type other
    -- than list.
    zettelEdges :: Zettel -> m [([Connection], ZettelID)]
    zettelEdges =
      fmap (fmap $ first pure) . outgoingLinks
    outgoingLinks :: Zettel -> m [(Connection, ZettelID)]
    outgoingLinks Zettel {..} =
      fmap concat $ for (extractLinks zettelContent) $ \mlink ->
        liftEither $ runExcept
          $ withExcept show
          $ liftEither (first (NeuronError_BadLink zettelID) $ neuronLinkFromMarkdownLink mlink) >>= \case
            Nothing ->
              pure []
            Just nlink -> do
              let conns = neuronLinkConnections zettels nlink
              -- Check the connections refer to existing zettels
              forM_ (snd <$> conns) $ \zref ->
                when (isNothing (Map.lookup zref zettelsMap))
                  $ throwError
                  $ NeuronError_BrokenZettelRef zettelID zref
              pure conns
