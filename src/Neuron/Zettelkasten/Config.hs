{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettelkasten config
module Neuron.Zettelkasten.Config where

import Data.FileEmbed (embedFile)
import Development.Shake (Action, readFile')
import Dhall (FromDhall)
import qualified Dhall
import Dhall.TH
import Path
import Path.IO (doesFileExist)
import Relude
import qualified Rib

makeHaskellTypes
  [ SingleConstructor "Config" "Config" "./src-dhall/Config/Type.dhall"
  ]

deriving instance Generic Config

deriving instance FromDhall Config

defaultConfig :: ByteString
defaultConfig = $(embedFile "./src-dhall/Config/Default.dhall")

getConfig :: Action Config
getConfig = do
  inputDir <- Rib.ribInputDir
  let configPath = inputDir </> configFile
  configVal :: Text <- doesFileExist configPath >>= \case
    True -> do
      userConfig <- fmap toText $ readFile' $ toFilePath configPath
      -- Dhall's combine operator (`//`) allows us to merge two records,
      -- effectively merging the record with defaults with the user record.
      pure $ decodeUtf8 defaultConfig <> " // " <> userConfig
    False ->
      pure $ decodeUtf8 @Text defaultConfig
  liftIO $ Dhall.detailed $ Dhall.input Dhall.auto configVal
  where
    configFile = [relfile|neuron.dhall|]
