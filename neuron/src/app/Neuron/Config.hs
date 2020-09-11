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

import Data.Set ((\\), member)
import Data.Either.Validation (validationToEither)
import Development.Shake (Action, readFile')
import Dhall (FromDhall)
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Dhall.Map
import Neuron.Config.Orphans ()
import Neuron.Config.Type (Config (..), configFile, configKeys, emptyConfig, mergeWithDefault)
import Relude
import Rib.Shake (ribInputDir)
import System.Directory
import System.FilePath

-- | Read the optional @neuron.dhall@ config file from the zettelkasten
getConfig :: Action Config
getConfig = do
  configPath <- ribInputDir <&> (</> configFile)
  configVal :: Text <-
    liftIO (doesFileExist configPath) >>= \case
      True ->
        toText <$> readFile' configPath
      False ->
        pure emptyConfig
  either fail pure $ parsePure configFile $ mergeWithDefault configVal

-- | Pure version of `Dhall.input Dhall.auto`
--
-- The config file cannot have imports, as that requires IO.
parsePure :: forall a. FromDhall a => FilePath -> Text -> Either String a
parsePure fn s = do
  expr0 <- first show $ Dhall.Parser.exprFromText fn s
  expr <- maybeToRight "Cannot have imports" $ traverse (const Nothing) expr0
  Dhall.Core.Record (Dhall.Map.keysSet -> fields) <- first show $ Dhall.TypeCheck.typeOf expr
  let spuriousFields = fields \\ configKeys
  case null spuriousFields of
    True ->
      first show $
        validationToEither $
          Dhall.extract @a Dhall.auto $
            Dhall.Core.normalize expr
    False -> do
      let spuriousMessage =
            "Configuration in neuron.dhall includes spurious fields: " <>
            intercalate ", " (("`" <>) . (<> "`") . toString <$> toList spuriousFields) <>
            "."
          -- TODO: remove once
          mathJaxMessage =
            if "mathJaxSupport" `member` spuriousFields
              then " To disable MathJax support, include a (possibly blank) `./head.html` file."
              else ""
      Left $ spuriousMessage <> mathJaxMessage
