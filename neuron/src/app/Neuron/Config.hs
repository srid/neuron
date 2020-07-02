{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Config
  ( getConfig,
    parsePure,
  )
where

import Control.Monad.Except
import Data.Either.Validation (validationToEither)
import Development.Shake (Action, readFile')
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.Substitution
import qualified Dhall.TypeCheck
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
parseConfig =
  -- liftIO . Dhall.input Dhall.auto
  pure . parsePure

-- WIP
parsePure :: Text -> Config
parsePure cfgText =
  either (error . show) id
    $ validationToEither
    $ Dhall.extract @Config Dhall.auto
    $ Dhall.Core.normalize
    $ either (error . show) id
    $ (\a -> a <$ Dhall.TypeCheck.typeOf a)
    $ flip Dhall.Substitution.substitute Dhall.Substitution.empty
    $ fromMaybe (error "imports")
    $ traverse (\_ -> Nothing)
    $ either (error . show) id
    $ Dhall.Parser.exprFromText "neuron.dhall" cfgText
