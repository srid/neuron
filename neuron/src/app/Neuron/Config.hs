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
    parseConfig,
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
import Neuron.Config.Type (Config (..), configFile, configKeys, mergeWithDefault)
import Relude
import Rib.Shake (ribInputDir)
import System.Directory
import System.FilePath

-- | Read the optional @neuron.dhall@ config file from the zettelkasten
getConfig :: Action Config
getConfig = do
  configPath <- ribInputDir <&> (</> configFile)
  configVal :: Maybe Text <-
    liftIO (doesFileExist configPath) >>= \case
      True ->
        Just . toText <$> readFile' configPath
      False ->
        pure Nothing
  either fail pure $ parseConfig configFile $ mergeWithDefault configVal

-- | Pure version of @'Dhall.input' 'Dhall.auto'@ that also returns the type.
--
-- The parsed file cannot have imports, as that requires IO.
parsePure :: FromDhall a => FilePath -> Text -> Either String (a, Dhall.Core.Expr Dhall.Parser.Src Void)
parsePure fn s = do
  expr0 <- first show $ Dhall.Parser.exprFromText fn s
  expr <- maybeToRight "Cannot have imports" $ traverse (const Nothing) expr0
  typ <- first show $ Dhall.TypeCheck.typeOf expr
  val <- first show $ validationToEither $ Dhall.extract Dhall.auto $ Dhall.Core.normalize expr
  pure (val, typ)

-- | Parse a Neuron Dhall config file.
--
-- The config file cannot have imports, as that requires IO.
-- The config file also cannot have spurious fields outside of those defined in 'Config'.
-- (Exception: for backwards compatibility, silently ignore a spurious @mathJaxSupport@ field
-- if it is the only spurious field.)
parseConfig :: FilePath -> Text -> Either String Config
parseConfig fn s = do
  (config, Dhall.Core.Record (Dhall.Map.keysSet -> fields)) <- parsePure fn s
  let spuriousFields = fields \\ configKeys
  if null $ spuriousFields \\ one "mathJaxSupport"
    then pure config
    else do
      let spuriousMessage =
            "Configuration in neuron.dhall includes spurious fields: " <>
            intercalate ", " (("`" <>) . (<> "`") . toString <$> toList spuriousFields) <> "."
          mathJaxMessage =
            if "mathJaxSupport" `member` spuriousFields
              then " To disable MathJax support, include a (possibly blank) `./head.html` file."
              else ""
      fail $ spuriousMessage <> mathJaxMessage
