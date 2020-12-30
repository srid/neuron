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

import Data.Either.Validation (validationToEither)
import qualified Data.Text as T
import Dhall (FromDhall)
import qualified Dhall (Decoder (extract), auto)
import qualified Dhall.Core (normalize)
import qualified Dhall.Parser (exprFromText)
import qualified Dhall.TypeCheck (typeOf)
import Neuron.CLI.Types (MonadApp, getNotesDir, needFile)
import Neuron.Config.Orphans ()
import Neuron.Config.Type (Config, configFile, defaultConfig, mergeWithDefault)
import Relude
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Read the optional @neuron.dhall@ config file from the zettelkasten
getConfig :: (MonadIO m, MonadFail m, MonadApp m) => m Config
getConfig = do
  notesDir <- getNotesDir
  let configPath = notesDir </> configFile
  configVal :: Text <-
    liftIO (doesFileExist configPath) >>= \case
      True -> do
        needFile configPath
        s <- readFileText configPath
        -- Accept empty neuron.dhall (used to signify a directory to be used with neuron)
        if T.null (T.strip s)
          then pure defaultConfig
          else pure $ mergeWithDefault s
      False ->
        fail $ "not a neuron notes directory (no neuron.dhall found under " <> notesDir <> ")"
  either fail pure $ parsePure configFile $ mergeWithDefault configVal

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
