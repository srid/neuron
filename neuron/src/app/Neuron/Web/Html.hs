{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Html where

import Control.Monad.Fix (MonadFix)
import Data.FileEmbed (embedOneStringFileOf)
import Neuron.Web.Cache.Type (NeuronCache (..))
import Neuron.Web.HeadHtml (HeadHtml, renderHeadHtml)
import Neuron.Web.Manifest (Manifest, renderManifest)
import Neuron.Web.Route (Route (..), routeConfig, runNeuronWeb)
import qualified Neuron.Web.Route.Data as RD
import Neuron.Web.StructuredData (renderStructuredData)
import qualified Neuron.Web.View as V
import qualified Neuron.Web.Widget as W
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude
import qualified Skylighting.Core as Skylighting

-- | The contents of GHCJS compiled JS.
--
-- We specify an alternate path, that is relative to project root, so that
-- ghcide will be able to compile this module.
impulseJS :: Text
impulseJS = $(embedOneStringFileOf ["./ghcjs/impulse.js", "./neuron/ghcjs/impulse.js"])

-- | Render the given route
renderRoutePage ::
  forall t m js a.
  ( PandocBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender js t m,
    PerformEvent t m,
    TriggerEvent t m
  ) =>
  Dynamic t (W.LoadableData NeuronCache) ->
  RD.RouteDataCache ->
  HeadHtml ->
  Manifest ->
  Route a ->
  m ()
renderRoutePage cacheDyn rdCache headHtml manifest r = do
  let mkRD = \ch -> RD.mkRouteData rdCache ch r
      fffmap = fmap . fmap . fmap
  el "html" $ do
    el "head" $ do
      -- TODO: replace passing config (siteTitle), with using RD
      -- Actually ... put version, theme, siteTitle (config stuff) in one common
      -- dyn, and rebuild all. Actually, just rebuild all on any neuron.dhall
      -- change. Wait a sec, just split `NeuronCache` into neuron.dhall/neuron
      -- (cfg+ver) fields, and zk field (graph+errs), then holdDyn on them.
      V.headTemplate r $ (_neuronCache_config &&& mkRD) `fffmap` cacheDyn
      renderHeadHtml headHtml
      renderManifest manifest
      W.loadingWidget' cacheDyn blank (const blank) $ \cacheDyn' ->
        dyn_ $
          ffor cacheDyn' $ \cache@NeuronCache {..} ->
            -- TODO: replace passing config and graph, with using RD (breadcrumb and opengraph)
            renderStructuredData _neuronCache_config r (_neuronCache_graph, mkRD cache)
      () <- case r of
        Route_Impulse {} ->
          elImpulseJS
        Route_ImpulseStatic ->
          blank
        Route_Zettel {} -> do
          elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
      pure ()
    el "body" $ do
      () <- case r of
        Route_Impulse {} -> do
          runNeuronWeb routeConfig $
            V.renderRouteImpulse $ mkRD `fffmap` cacheDyn
        Route_ImpulseStatic {} -> do
          runNeuronWeb routeConfig $
            V.renderRouteImpulse $ mkRD `fffmap` cacheDyn
        Route_Zettel {} -> do
          runNeuronWeb routeConfig $
            -- TODO: replace passing config and graph, with using RD
            V.renderRouteZettel $ (id &&& mkRD) `fffmap` cacheDyn
      pure ()

elImpulseJS :: DomBuilder t m => m ()
elImpulseJS = do
  -- XXX: Disabling JSON cache, because we don't yet know of a performant
  -- way to load it in GHCJS.
  -- ...
  -- The JSON cache being injected here will be accessed at runtime by
  -- Impulse. It is also available on disk as `cache.json`, which Impulse
  -- retrieves in development mode (as no injection can happen in the
  -- GHC/jsaddle context).
  {-
  let cacheJsonJson =
        TL.toStrict $
          encodeToLazyText $
            TL.toStrict $ encodeToLazyText cache
  el "script" $ text $ "\nvar cacheText = " <> cacheJsonJson <> ";\n"
  -- el "script" $ text $ "\nvar cache = " <> (TL.toStrict . encodeToLazyText) cache <> ";\n"
  -}
  el "script" $ text impulseJS