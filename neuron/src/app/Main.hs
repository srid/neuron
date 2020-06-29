{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad.Reader
import Development.Shake
import Main.Utf8
import Neuron.CLI (run)
import qualified Neuron.Config as Config
import Neuron.Web.Generate (generateSite)
import Neuron.Web.Generate.Route (staticRouteConfig)
import qualified Neuron.Web.Manifest as Manifest
import Neuron.Web.Route (Route (..), runNeuronWeb)
import Neuron.Web.View (renderRoutePage)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Reflex.Dom.Core
import Relude
import qualified Rib

main :: IO ()
main = withUtf8 $ run generateMainSite

generateMainSite :: Action ()
generateMainSite = do
  notesDir <- Rib.ribInputDir
  Rib.buildStaticFiles ["static/**", ".nojekyll"]
  config <- Config.getConfig
  manifest <- fmap Manifest.mkManifest $ getDirectoryFiles notesDir Manifest.manifestPatterns
  let writeHtmlRoute :: Route a -> (ZettelGraph, a) -> Action ()
      writeHtmlRoute r x = do
        html <- liftIO $ fmap snd $ renderStatic $ do
          runNeuronWeb staticRouteConfig $
            renderRoutePage config manifest r x
        -- FIXME: Make rib take bytestrings
        Rib.writeRoute r $ decodeUtf8 @Text html
  void $ generateSite config writeHtmlRoute
