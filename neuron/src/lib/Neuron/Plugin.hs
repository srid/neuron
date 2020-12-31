{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin where

import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import Data.Some (Some (..), withSome)
import Neuron.Plugin.PluginData (PluginData (..))
import qualified Neuron.Plugin.Plugins.DirTree as DirTree
import qualified Neuron.Plugin.Plugins.NeuronIgnore as NeuronIgnore
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel (ZettelC)
import Neuron.Zettelkasten.Zettel.Parser (extractQueriesWithContext, parseZettels)
import Reflex.Dom.Core (DomBuilder)
import Reflex.Dom.Widget (blank)
import Relude
import qualified System.Directory.Contents.Types as DC

type PluginRegistry = Map (Some PluginData) (Some Plugin)

lookupPlugins :: [Text] -> PluginRegistry
lookupPlugins = Map.fromList . mapMaybe lookupPlugin
  where
    lookupPlugin :: Text -> Maybe (Some PluginData, Some Plugin)
    lookupPlugin = \case
      "dirtree" -> Just (Some PluginData_DirTree, Some DirTree.plugin)
      "neuronignore" -> Just (Some PluginData_NeuronIgnore, Some NeuronIgnore.plugin)
      _ -> Nothing

filterSources :: PluginRegistry -> DC.DirTree FilePath -> IO (Maybe (DC.DirTree FilePath))
filterSources plugins t = do
  let applyF = Map.elems plugins <&> \sp -> withSome sp _plugin_filterSources
      combiner :: (Traversable m, Monad m) => IO (m a) -> (a -> IO (m b)) -> IO (m b)
      combiner = \ima f -> (fmap join . traverse f) =<< ima
  foldl' combiner (pure $ Just t) applyF

afterZettelParse :: PluginRegistry -> [(ZettelID, (FilePath, (Text, DMap PluginData Identity)))] -> [ZettelC]
afterZettelParse plugins fs = do
  parseZettels extractQueriesWithContext fs <&> \z ->
    foldl' (\z1 f -> f z1) z (plugins <&> \sp -> withSome sp _plugin_afterZettelParse)

afterZettelRead :: MonadState (Map ZettelID ZIDRef) m => PluginRegistry -> DC.DirTree FilePath -> m ()
afterZettelRead plugins fileTree = do
  forM_ (plugins <&> \sp -> withSome sp _plugin_afterZettelRead) $ \f -> f fileTree

renderPluginPanel :: DomBuilder t m => ZettelGraph -> DSum PluginData Identity -> NeuronWebT t m ()
renderPluginPanel graph = \case
  PluginData_DirTree :=> Identity t ->
    DirTree.renderPanel graph t
  PluginData_NeuronIgnore :=> Identity () ->
    blank
