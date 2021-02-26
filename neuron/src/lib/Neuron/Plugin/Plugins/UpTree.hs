{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Plugins.UpTree (plugin, routePluginData, render) where

import qualified Data.Dependent.Map as DMap
import Data.Tree (Forest)
import Neuron.Frontend.Route (NeuronWebT)
import qualified Neuron.Frontend.Widget.InvertedTree as IT
import qualified Neuron.Plugin.Plugins.Links as Links
import Neuron.Plugin.Type (Plugin (..))
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Relude hiding (trace, traceShow, traceShowId)

plugin :: Plugin (Forest Zettel)
plugin =
  def
    { _plugin_afterZettelParse = bimap enable enable,
      _plugin_routeData = routePluginData,
      _plugin_renderPanel = const render,
      _plugin_css = const IT.style
    }

enable :: ZettelT c -> ZettelT c
enable z =
  z {zettelPluginData = DMap.insert UpTree (Identity ()) (zettelPluginData z)}

routePluginData :: ZettelGraph -> ZettelC -> Forest Zettel
routePluginData g (sansContent -> z) =
  G.uplinkForest z g

render :: (DomBuilder t m, PostBuild t m) => Forest Zettel -> NeuronWebT t m ()
render upTree = do
  unless (null upTree) $ do
    IT.renderInvertedHeadlessTree "zettel-uptree" "deemphasized" upTree $ \z2 ->
      Links.renderZettelLink Nothing Nothing def z2
