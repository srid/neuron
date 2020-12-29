{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Data.FileEmbed (embedOneStringFileOf)
import Development.Shake (Action, getDirectoryFiles)
import Main.Utf8 (withUtf8)
import Neuron.CLI (run)
import Neuron.Config.Type (Config)
import Neuron.Web.Cache.Type (NeuronCache (..))
import Neuron.Web.Generate (generateSite)
import Neuron.Web.HeadHtml (HeadHtml, getHeadHtml, renderHeadHtml)
import Neuron.Web.Manifest (Manifest, renderManifest)
import qualified Neuron.Web.Manifest as Manifest
import Neuron.Web.Route (Route (..), routeConfig, runNeuronWeb)
import qualified Neuron.Web.Route.Data as RD
import Neuron.Web.StructuredData (renderStructuredData)
import qualified Neuron.Web.View as V
import qualified Neuron.Web.Widget as W
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude
import Rib.Route (writeRoute)
import Rib.Shake (buildStaticFiles, ribInputDir)
import qualified Skylighting.Core as Skylighting

type App t m js =
  ( PandocBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender js t m,
    PerformEvent t m,
    TriggerEvent t m
  )

-- | The contents of GHCJS compiled JS.
--
-- We specify an alternate path, that is relative to project root, so that
-- ghcide will be able to compile this module.
impulseJS :: Text
impulseJS = $(embedOneStringFileOf ["./ghcjs/impulse.js", "./neuron/ghcjs/impulse.js"])

main :: IO ()
main = withUtf8 $ run generateMainSite

generateMainSite :: Config -> Action ()
generateMainSite config = do
  notesDir <- ribInputDir
  -- TODO: Make "static/*" configurable in .dhall; `{ staticIncludes = "static/*"; }`
  buildStaticFiles ["static/**", ".nojekyll"]
  manifest <- Manifest.mkManifest <$> getDirectoryFiles notesDir Manifest.manifestPatterns
  headHtml <- getHeadHtml
  let writeHtmlRoute :: NeuronCache -> RD.RouteDataCache -> Route a -> Action ()
      writeHtmlRoute cache rdCache r = do
        let w :: App t m js => m ()
            w = do
              let cacheDyn = case r of
                    Route_Impulse {} ->
                      -- FIXME: Injecting initial value here will break hydration on Impulse.
                      constDyn Nothing
                    _ ->
                      constDyn $ W.availableData cache
              renderRoutePage cacheDyn rdCache headHtml manifest r
        -- We do this verbose dance to make sure hydration happens only on Impulse route.
        -- Ideally, this should be abstracted out, but polymorphic types are a bitch.
        html :: ByteString <- liftIO $ case r of
          Route_Impulse {} ->
            fmap snd . renderStatic . runHydratableT $ w
          _ ->
            fmap snd . renderStatic $ w
        -- DOCTYPE declaration is helpful for code that might appear in the user's `head.html` file (e.g. KaTeX).
        writeRoute r $ decodeUtf8 @Text $ "<!DOCTYPE html>" <> html
  void $ generateSite config writeHtmlRoute

-- | Render the given route
renderRoutePage ::
  forall t m js a.
  App t m js =>
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
      V.headTemplate r $ (_neuronCache_config &&& mkRD) `fffmap` cacheDyn
      renderHeadHtml headHtml
      renderManifest manifest
      W.loadingWidget' cacheDyn blank (const blank) $ \cacheDyn' ->
        dyn_ $
          ffor cacheDyn' $ \cache@NeuronCache {..} ->
            -- TODO: replace passing graph, with using RD
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
            -- TODO: replace passing graph, with using RD
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