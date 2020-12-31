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

import Control.Concurrent.Async (race_)
import Development.Shake (Action, getDirectoryFiles)
import GHC.IO.Handle (BufferMode (LineBuffering))
import Main.Utf8 (withUtf8)
import qualified Neuron.Backend as Backend
import Neuron.CLI (run)
import Neuron.CLI.Types (MonadApp (getNotesDir), getApp, getOutputDir, runAppT)
import Neuron.Config.Type (Config)
import qualified Neuron.Gen as Gen
import Neuron.Web.Cache.Type (NeuronCache (..))
import Neuron.Web.Generate (generateSite)
import Neuron.Web.HeadHtml (getHeadHtml)
import qualified Neuron.Web.Html as Html
import qualified Neuron.Web.Manifest as Manifest
import Neuron.Web.Route (Route (..))
import qualified Neuron.Web.Route.Data as RD
import qualified Neuron.Web.Widget as W
import Reflex.Dom.Core
import Relude
import Rib.Shake (buildStaticFiles)
import System.IO (hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  withUtf8 $
    run generateMainSite $ do
      outputDir <- getOutputDir
      app <- getApp
      liftIO $ race_ (runAppT app Gen.generateSite) (Backend.serve "127.0.0.1" 8080 outputDir)

generateMainSite :: Config -> Action ()
generateMainSite config = do
  -- TODO: Make "static/*" configurable in .dhall; `{ staticIncludes = "static/*"; }`
  buildStaticFiles ["static/**", ".nojekyll"]
  headHtml <- getHeadHtml
  manifest <-
    Manifest.mkManifest <$> do
      notesDir <- getNotesDir
      getDirectoryFiles notesDir Manifest.manifestPatterns
  let writeHtmlRoute :: NeuronCache -> RD.RouteDataCache -> Route a -> Action ()
      writeHtmlRoute cache rdCache r = do
        -- We do this verbose dance to make sure hydration happens only on Impulse route.
        -- Ideally, this should be abstracted out, but polymorphic types are a bitch.
        html :: ByteString <- liftIO $ case r of
          Route_Impulse {} ->
            fmap snd . renderStatic . runHydratableT $ do
              -- FIXME: Injecting initial value here will break hydration on Impulse.
              let cacheDyn = constDyn $ W.LoadableData Nothing
              Html.renderRoutePage cacheDyn rdCache headHtml manifest r
          _ ->
            fmap snd . renderStatic $ do
              let cacheDyn = constDyn $ W.availableData cache
              Html.renderRoutePage cacheDyn rdCache headHtml manifest r
        Gen.writeRouteHtml r html
  void $ generateSite config writeHtmlRoute
