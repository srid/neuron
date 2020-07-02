{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Config
  ( getConfig,
  )
where

import Control.Monad.Except
import Data.FileEmbed (embedFile)
import Development.Shake (Action, readFile')
import qualified Dhall
import Neuron.Config.Orphans ()
import Neuron.Config.Type (Config, configFile, mergeWithDefault)
import Relude
import qualified Rib
import System.Directory
import System.FilePath

-- | Read the optional @neuron.dhall@ config file from the zettelksaten
getConfig :: Action Config
getConfig = do
  inputDir <- Rib.ribInputDir
  let configPath = inputDir </> configFile
  configVal :: Text <- liftIO (doesFileExist configPath) >>= \case
    True -> do
      fmap toText $ readFile' configPath
    False ->
      pure "{}"
  parseConfig $ mergeWithDefault configVal

parseConfig :: MonadIO m => Text -> m Config
parseConfig s =
  liftIO $ Dhall.input Dhall.auto s
