{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin where

import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import Data.Some (Some (..))
import Neuron.Plugin.PluginData (PluginData (..))
import qualified Neuron.Plugin.Plugins.DirTree as DirTree
import Neuron.Plugin.Type (Plugin)
import Neuron.Web.Route (NeuronWebT)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Reflex.Dom.Core (DomBuilder)
import Relude

type PluginRegistry = Map (Some PluginData) (Some Plugin)

lookupPlugins :: [Text] -> PluginRegistry
lookupPlugins = Map.fromList . mapMaybe lookupPlugin
  where
    lookupPlugin :: Text -> Maybe (Some PluginData, Some Plugin)
    lookupPlugin = \case
      "dirtree" -> Just (Some PluginData_DirTree, Some DirTree.plugin)
      _ -> Nothing

renderPluginPanel :: DomBuilder t m => ZettelGraph -> DSum PluginData Identity -> NeuronWebT t m ()
renderPluginPanel graph = \case
  PluginData_DirTree :=> Identity t ->
    DirTree.renderPanel graph t
