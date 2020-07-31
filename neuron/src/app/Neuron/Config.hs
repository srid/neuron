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
import Development.Shake (Action, readFile')
import qualified Dhall
import Dhall (FromDhall)
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import Neuron.Config.Orphans ()
import Neuron.Config.Type (Config, configFile, defaultConfig, mergeWithDefault)
import Relude
import Rib.Shake (ribInputDir)
import System.Directory
import System.FilePath

-- | Read the optional @neuron.dhall@ config file from the zettelkasten
getConfig :: Action Config
getConfig = do
  configPath <- ribInputDir <&> (</> configFile)
  configVal :: Text <- liftIO (doesFileExist configPath) >>= \case
    True -> do
      mergeWithDefault . toText <$> readFile' configPath
    False ->
      pure defaultConfig
  either fail pure $ parsePure configFile $ mergeWithDefault configVal

-- | Pure version of `Dhall.input Dhall.auto`
--
-- The config file cannot have imports, as that requires IO.
parsePure :: forall a. FromDhall a => FilePath -> Text -> Either String a
parsePure fn s = do
  expr0 <- first show $ Dhall.Parser.exprFromText fn s
  expr <- maybeToRight "Cannot have imports" $ traverse (const Nothing) expr0
  void $ first show $ Dhall.TypeCheck.typeOf expr
  first show
    $ validationToEither
    $ Dhall.extract @a Dhall.auto
    $ Dhall.Core.normalize expr
