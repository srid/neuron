{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Development.Shake (Action, getDirectoryFiles)
import Main.Utf8 (withUtf8)
import Neuron.CLI (run)
import Neuron.Config.Type (Config)
import Neuron.Version (neuronVersion)
import Neuron.Web.Generate (generateSite)
import Neuron.Web.Generate.Route (staticRouteConfig)
import Neuron.Web.HeadHtml (HeadHtml, getHeadHtml, renderHeadHtml)
import Neuron.Web.Manifest (Manifest, renderManifest)
import qualified Neuron.Web.Manifest as Manifest
import Neuron.Web.Route (NeuronWebT, Route (..), runNeuronWeb)
import Neuron.Web.StructuredData (renderStructuredData)
import Neuron.Web.View (headTemplate, renderRouteBody, routeTitle)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude
import Rib.Route (writeRoute)
import Rib.Shake (buildStaticFiles, ribInputDir)
import qualified Skylighting.Core as Skylighting

main :: IO ()
main = withUtf8 $ run generateMainSite

generateMainSite :: Config -> Action ()
generateMainSite config = do
  notesDir <- ribInputDir
  buildStaticFiles ["static/**", ".nojekyll"]
  manifest <- Manifest.mkManifest <$> getDirectoryFiles notesDir Manifest.manifestPatterns
  headHtml <- getHeadHtml
  let writeHtmlRoute :: Route a -> (ZettelGraph, a) -> Action ()
      writeHtmlRoute r x = do
        html <- liftIO $
          fmap snd $
            renderStatic $ do
              runNeuronWeb staticRouteConfig $
                renderRoutePage config headHtml manifest r x
        -- FIXME: Make rib take bytestrings
        writeRoute r $ decodeUtf8 @Text html
  void $ generateSite config writeHtmlRoute

-- | Render the given route
renderRoutePage ::
  (PandocBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  Config ->
  HeadHtml ->
  Manifest ->
  Route a ->
  (ZettelGraph, a) ->
  NeuronWebT t m ()
renderRoutePage config headHtml manifest r val = do
  -- DOCTYPE declaration is helpful for code that might appear in the user's `head.html` file (e.g. KaTeX).
  el "!DOCTYPE html" blank
  elAttr "html" ("lang" =: "en") $ do
    el "head" $ do
      headTemplate (text $ routeTitle config (snd val) r) $ do
        case r of
          Route_Zettel _ -> do
            renderHeadHtml headHtml
            renderManifest manifest
            renderStructuredData config r val
            elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
          _ -> blank
    el "body" $ do
      renderRouteBody neuronVersion config r val