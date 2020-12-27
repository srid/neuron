{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neuron.Plugin where

import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity (Identity (..))
import Data.Some (Some (..))
import Neuron.Plugin.PluginData (PluginData (..))
import qualified Neuron.Plugin.Plugins.DirectoryFolgezettel as DF
import Neuron.Plugin.Type (Plugin)
import Neuron.Web.Route (NeuronWebT)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Reflex.Dom.Core (DomBuilder)

-- | Enabled neuron plugins
-- TODO: allow it to be specified in neuron.dhall
plugins :: [Some Plugin]
plugins =
  [ Some DF.plugin
  ]

renderPluginPanel :: DomBuilder t m => ZettelGraph -> DSum PluginData Identity -> NeuronWebT t m ()
renderPluginPanel graph = \case
  PluginData_DirectoryZettel :=> Identity t ->
    DF.renderPanel graph t
