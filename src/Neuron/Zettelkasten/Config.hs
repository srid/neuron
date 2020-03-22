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

import Development.Shake (Action)
import Dhall.TH
import Path
import Path.IO (doesFileExist)
import Relude
import qualified Rib
import qualified Rib.Parser.Dhall as Dhall

makeHaskellTypes
  [ SingleConstructor "Config" "Config" "./src-dhall/Neuron.dhall"
  ]

getConfig :: Action Config
getConfig = do
  inputDir <- Rib.ribInputDir
  doesFileExist (inputDir </> configPath) >>= \case
    True -> Dhall.parse [] configPath
    False -> pure defaultConfig
  where
    configPath = [relfile|neuron.dhall|]
    defaultConfig =
      Config
        { siteTitle = "My Zettelkasten",
          author = mempty,
          siteBaseUrl = mempty
        }
