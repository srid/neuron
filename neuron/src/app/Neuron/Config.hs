{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- For deriving of FromDhall Config
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Config
  ( getConfigFromFile,
    onMissingConfigFile,
    parsePure,
  )
where

import Colog (WithLog, log)
import Data.Either.Validation (validationToEither)
import qualified Data.Text as T
import Dhall (FromDhall)
import qualified Dhall (Decoder (extract), auto)
import qualified Dhall.Core (normalize)
import qualified Dhall.Parser (exprFromText)
import qualified Dhall.TypeCheck (typeOf)
import Neuron.CLI.Logging
import Neuron.CLI.Types (MonadApp, getNotesDir)
import Neuron.Config.Type (Config, configFile, defaultConfig, mergeWithDefault)
import Relude

deriving instance FromDhall Config

getConfigFromFile :: (MonadIO m, MonadFail m) => FilePath -> m Config
getConfigFromFile configPath = do
  s <- readFileText configPath
  -- Accept empty neuron.dhall (used to signify a directory to be used with neuron)
  let configVal =
        if T.null (T.strip s)
          then defaultConfig
          else mergeWithDefault s
  either fail pure $
    parsePure configFile $ mergeWithDefault configVal

-- TODO: Start usin this in reflex app
onMissingConfigFile :: (MonadFail m, MonadApp m, WithLog env Message m) => m a
onMissingConfigFile = do
  notesDir <- getNotesDir
  log E $ "You must add a neuron.dhall to " <> toText notesDir
  log E "You can add one by running:"
  log E $ "  touch " <> toText notesDir <> "/neuron.dhall"
  fail "Not a neuron notes directory"

-- | Pure version of `Dhall.input Dhall.auto`
--
-- The config file cannot have imports, as that requires IO.
parsePure :: forall a. FromDhall a => FilePath -> Text -> Either String a
parsePure fn s = do
  expr0 <- first show $ Dhall.Parser.exprFromText fn s
  expr <- maybeToRight "Cannot have imports" $ traverse (const Nothing) expr0
  void $ first show $ Dhall.TypeCheck.typeOf expr
  first show $
    validationToEither $
      Dhall.extract @a Dhall.auto $
        Dhall.Core.normalize expr
