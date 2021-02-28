{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Plugins.UpTree (plugin, routePluginData, render, renderZettelHead) where

import Data.Some
import qualified Data.Structured.Breadcrumb as Breadcrumb
import Data.Tree (Forest)
import Neuron.Frontend.Route (NeuronWebT)
import qualified Neuron.Frontend.Route as R
import Neuron.Frontend.Route.Data.Types
import qualified Neuron.Frontend.Route.Data.Types as R
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
      -- _plugin_routeData = routePluginData,
      _plugin_renderPanel = const render,
      _plugin_css = const IT.style
    }

enable :: ZettelT c -> ZettelT c
enable =
  setPluginData UpTree ()

routePluginData :: ZettelGraph -> ZettelC -> Forest Zettel
routePluginData g (sansContent -> z) =
  G.uplinkForest z g

render :: (DomBuilder t m, PostBuild t m) => Forest Zettel -> NeuronWebT t m ()
render upTree = do
  unless (null upTree) $ do
    IT.renderInvertedHeadlessTree "zettel-uptree" "deemphasized" upTree $ \z2 ->
      Links.renderZettelLink Nothing Nothing def z2

-- Render breadcrumbs for uptree
renderZettelHead :: DomBuilder t m => R.RouteConfig t m -> (SiteData, ZettelData) -> Forest Zettel -> m ()
renderZettelHead routeCfg v uptree = do
  Breadcrumb.renderBreadcrumbs $ case R.siteDataSiteBaseUrl (fst v) of
    Nothing ->
      -- No base url set in neuron.dhall; nothing to do.
      []
    Just baseUrl ->
      let mkCrumb :: Zettel -> Breadcrumb.Item
          mkCrumb Zettel {..} =
            let zettelRelUrl = R.routeConfigRouteURL routeCfg (Some $ R.Route_Zettel zettelSlug)
             in Breadcrumb.Item zettelTitle (Just $ R.routeUri baseUrl zettelRelUrl)
       in Breadcrumb.fromForest $ fmap mkCrumb <$> uptree
