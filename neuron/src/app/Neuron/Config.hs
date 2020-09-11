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
import Dhall (FromDhall)
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import Neuron.Config.Orphans ()
import Neuron.Config.Type (Config (..), configFile, headHtmlFile, emptyConfig, mergeWithDefault)
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
  headHtmlPath <- ribInputDir <&> (</> headHtmlFile)
  headHtmlVal <-
    liftIO (doesFileExist headHtmlPath) >>= \case
      True ->
        Just . toText <$> readFile' headHtmlPath
      False ->
        pure Nothing
  config <- either fail pure $ parsePure configFile $ mergeWithDefault configVal
  case (headHtml config, headHtmlVal) of
    (Just _, Just _) ->
      fail "Can either include `head.html` file, or set `headHtml` to a `Some` value in `neuron.dhall`, but not both."
    (Nothing, Just _) ->
      pure config{headHtml = headHtmlVal}
    _ ->
      pure config

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
