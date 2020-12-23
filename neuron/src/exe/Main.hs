{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
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
import qualified Neuron.Web.Cache.Type as Cache
import Neuron.Web.Generate (generateSite)
import Neuron.Web.HeadHtml (HeadHtml, getHeadHtml, renderHeadHtml)
import Neuron.Web.Manifest (Manifest, renderManifest)
import qualified Neuron.Web.Manifest as Manifest
import Neuron.Web.Route (Route (..), routeConfig, runNeuronWeb)
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
  buildStaticFiles ["static/**", ".nojekyll"]
  manifest <- Manifest.mkManifest <$> getDirectoryFiles notesDir Manifest.manifestPatterns
  headHtml <- getHeadHtml
  let writeHtmlRoute :: NeuronCache -> Route a -> a -> Action ()
      writeHtmlRoute cache r x = do
        let w :: App t m js => m ()
            w = renderRoutePage cache headHtml manifest r x
        -- We do this verbose dance to make sure hydration happens only on Impulse route.
        -- Ideally, this should be abstracted out, but polymorphic types are a bitch.
        html :: ByteString <- liftIO $ case r of
          Route_Zettel {} ->
            fmap snd . renderStatic $ w
          Route_Impulse {} ->
            fmap snd . renderStatic . runHydratableT $ w
        -- DOCTYPE declaration is helpful for code that might appear in the user's `head.html` file (e.g. KaTeX).
        writeRoute r $ decodeUtf8 @Text $ "<!DOCTYPE html>" <> html
  void $ generateSite config writeHtmlRoute

-- | Render the given route
renderRoutePage ::
  forall t m js a.
  App t m js =>
  NeuronCache ->
  HeadHtml ->
  Manifest ->
  Route a ->
  a ->
  m ()
renderRoutePage cache@NeuronCache {..} headHtml manifest r val = do
  el "html" $ do
    rec el "head" $ do
          V.headTemplate (fmap (fmap Cache._neuronCache_config) <$> cacheDyn) r val
          () <- case r of
            Route_Zettel _ -> do
              -- These three common elements could impact hydration; however, even
              -- without them, hydration doesn't work in ghcjs for some reason. That
              -- needs to be investigated.
              -- TODO: Move to common again?
              renderHeadHtml headHtml
              renderManifest manifest
              renderStructuredData _neuronCache_config r (_neuronCache_graph, val)
              elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
            Route_Impulse {} ->
              elImpulseJS
          pure ()
        cacheDyn <- el "body" $ do
          case r of
            Route_Impulse {} -> do
              -- FIXME: Injecting initial value here will break hydration.
              c <- Cache.reflexDomGetCache Nothing -- (W.availableData cache)
              runNeuronWeb routeConfig $
                V.renderRouteImpulse c
              pure c
            Route_Zettel {} -> do
              c <- Cache.reflexDomGetCache $ W.availableData cache
              runNeuronWeb routeConfig $
                V.renderRouteZettel val c
              pure c
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