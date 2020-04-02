{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettelkasten config
module Neuron.Zettelkasten.Config where

import Development.Shake (Action, readFile')
import Dhall (auto, input)
import Dhall.TH
import Path
import Path.IO (doesFileExist)
import Relude
import qualified Rib

makeHaskellTypes
  [ SingleConstructor "Config" "Config" "./src-dhall/Config/Type.dhall"
  ]

getConfig :: Action Config
getConfig = do
  inputDir <- Rib.ribInputDir
  let configPath = inputDir </> configFile
  doesFileExist configPath >>= \case
    True -> do
      config <- readFile' $ toFilePath configPath
      let configFull = "let Neuron = ./src-dhall/Neuron.dhall in " <> config
      liftIO $ input auto (toText configFull)
    False -> pure defaultConfig
  where
    configFile = [relfile|neuron.dhall|]
    defaultConfig :: Config
    defaultConfig =
      Config
        { siteTitle = "Neuron Zettelkasten",
          author = mempty,
          siteBaseUrl = mempty,
          editUrl = mempty
        }
