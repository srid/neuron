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
import Neuron.Config.Type (Config)
import Neuron.Version (neuronVersion)
import Neuron.Web.Generate (generateSite)
import Neuron.Web.Generate.Route (staticRouteConfig)
import Neuron.Web.HeadHtml (getHeadHtml)
import qualified Neuron.Web.Manifest as Manifest
import Neuron.Web.Route (Route (..), runNeuronWeb)
import Neuron.Web.View (renderRoutePage)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Reflex.Dom.Core
import Relude
import Rib.Route (writeRoute)
import Rib.Shake (buildStaticFiles, ribInputDir)

main :: IO ()
main = withUtf8 $ run generateMainSite

generateMainSite :: Config -> Action ()
generateMainSite config = do
  notesDir <- ribInputDir
  buildStaticFiles ["static/**", ".nojekyll"]
  manifest <- fmap Manifest.mkManifest $ getDirectoryFiles notesDir Manifest.manifestPatterns
  headHtml <- getHeadHtml
  let writeHtmlRoute :: Route a -> (ZettelGraph, a) -> Action ()
      writeHtmlRoute r x = do
        html <- liftIO $
          fmap snd $
            renderStatic $ do
              runNeuronWeb staticRouteConfig $
                renderRoutePage neuronVersion config headHtml manifest r x
        -- FIXME: Make rib take bytestrings
        writeRoute r $ decodeUtf8 @Text html
  void $ generateSite config writeHtmlRoute
