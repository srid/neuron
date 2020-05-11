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
  ( Config (..),
    configFile,
    getConfig,
    BaseUrlError (..),
  )
where

import Control.Monad.Except
import Data.FileEmbed (embedFile)
import Development.Shake (Action, readFile')
import Dhall (FromDhall)
import qualified Dhall
import Dhall.TH
import Relude
import qualified Rib
import System.Directory
import System.FilePath

-- | Config type for @neuron.dhall@
--
-- See <https://neuron.zettel.page/2011701.html guide> for description of the fields.
makeHaskellTypes
  [ SingleConstructor "Config" "Config" "./src-dhall/Config/Type.dhall"
  ]

deriving instance Generic Config

deriving instance FromDhall Config

data BaseUrlError
  = BaseUrlNotAbsolute
  deriving (Eq, Show)

instance Exception BaseUrlError

defaultConfig :: ByteString
defaultConfig = $(embedFile "./src-dhall/Config/Default.dhall")

configFile :: FilePath
configFile = "neuron.dhall"

-- | Read the optional @neuron.dhall@ config file from the zettelksaten
getConfig :: Action Config
getConfig = do
  inputDir <- Rib.ribInputDir
  let configPath = inputDir </> configFile
  configVal :: Text <- liftIO (doesFileExist configPath) >>= \case
    True -> do
      userConfig <- fmap toText $ readFile' configPath
      -- Dhall's combine operator (`//`) allows us to merge two records,
      -- effectively merging the record with defaults with the user record.
      pure $ decodeUtf8 defaultConfig <> " // " <> userConfig
    False ->
      pure $ decodeUtf8 @Text defaultConfig
  parseConfig configVal

parseConfig :: MonadIO m => Text -> m Config
parseConfig s =
  liftIO $ Dhall.input Dhall.auto s
