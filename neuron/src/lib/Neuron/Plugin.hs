{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neuron.Plugin where

import Data.Dependent.Sum (DSum (..))
import Data.Some (Some (..))
import qualified Neuron.Plugin.DirectoryFolgezettel as DF
import Neuron.Plugin.Type (Plugin)
import Neuron.Web.Route (NeuronWebT)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.Zettel (ZettelPluginData (..))
import Reflex.Dom.Core (DomBuilder, blank)

-- | Enabled neuron plugins
-- TODO: allow it to be specified in neuron.dhall
plugins :: [Some Plugin]
plugins =
  [ Some DF.plugin
  ]

renderPluginPanel :: DomBuilder t m => ZettelGraph -> DSum ZettelPluginData Maybe -> NeuronWebT t m ()
renderPluginPanel graph = \case
  ZettelPluginData_DirectoryZettel :=> Just t ->
    DF.renderPanel graph t
  ZettelPluginData_DirectoryZettel :=> _ ->
    blank
