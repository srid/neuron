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
import GHC.IO.Handle (BufferMode (LineBuffering))
import Main.Utf8 (withUtf8)
import qualified Neuron.Backend as Backend
import Neuron.CLI (run)
import Neuron.CLI.Types (getApp, getOutputDir, runAppT)
import qualified Neuron.Gen as Gen
import Relude
import System.IO (hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  withUtf8 $
    run $ do
      outputDir <- getOutputDir
      app <- getApp
      liftIO $ race_ (runAppT app Gen.generateSite) (Backend.serve "127.0.0.1" 8080 outputDir)
