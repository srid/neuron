{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettelkasten config
module Neuron.Zettelkasten.Config where

import Development.Shake (Action, readFile')
import Dhall (FromDhall)
import qualified Dhall
import qualified Dhall.Context
import Dhall.Import (load)
import Dhall.Parser (exprFromText)
import Dhall.TH
import GHC.Stack
import qualified Lens.Family as Lens
import Path
import Path.IO (doesFileExist, withCurrentDir)
import Relude
import qualified Rib

makeHaskellTypes
  [ SingleConstructor "Config" "Config" "./src-dhall/Config/Type.dhall",
    SingleConstructor "Neuron" "Neuron" "./src-dhall/Neuron.dhall"
  ]

deriving instance Generic Config

deriving instance FromDhall Config

getConfig :: HasCallStack => Action Config
getConfig = do
  inputDir <- Rib.ribInputDir
  let configPath = inputDir </> configFile
  doesFileExist configPath >>= \case
    True -> do
      config <- readFile' $ toFilePath configPath
      -- FIXME: Can't access the filesystem at runtime (outside of src tree)
      expr <- liftIO $ withCurrentDir [reldir|src-dhall|] $ do
        load =<< fmap (either (error . show) id . exprFromText "Neuron.dhall" . toText) (readFile "./Neuron.dhall")
      let startingContext = transform Dhall.Context.empty
            where
              transform = Dhall.Context.insert "Neuron" expr
      let inputSettings = transform Dhall.defaultInputSettings
            where
              transform =
                Lens.set Dhall.startingContext startingContext
      liftIO $ Dhall.detailed $ Dhall.inputWithSettings inputSettings Dhall.auto $ toText config
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
