{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Config.Type
  ( Config (..),
    configFile,
    defaultConfig,
    mergeWithDefault,
    getSiteBaseUrl,
    getPlugins,
  )
where

import Data.Aeson
import Neuron.Plugin (PluginRegistry)
import qualified Neuron.Plugin as Plugin
import Relude hiding (readEither)
import Text.URI (URI, mkURI)

configFile :: FilePath
configFile = "neuron.dhall"

-- | Config type for @neuron.dhall@
--
-- See <https://neuron.zettel.page/configuration.html guide> for description of the fields.
--
-- TODO: Implement custom `FromDhall` instance, while using original field types
data Config = Config
  { author :: Maybe Text,
    editUrl :: Maybe Text,
    siteBaseUrl :: Maybe Text,
    siteTitle :: Text,
    theme :: Text,
    plugins :: [Text]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

getSiteBaseUrl :: MonadFail m => Config -> m (Maybe URI)
getSiteBaseUrl Config {..} =
  runMaybeT $ do
    s <- MaybeT $ pure siteBaseUrl
    case mkURI s of
      Left e ->
        fail $ displayException e
      Right uri ->
        pure uri

getPlugins :: Config -> PluginRegistry
getPlugins = Plugin.lookupPlugins . plugins

defaultConfig :: Text
defaultConfig =
  "{ siteTitle =\
  \   \"My Zettelkasten\" \
  \, author =\
  \   None Text\
  \, siteBaseUrl =\
  \   None Text\
  \, editUrl =\
  \   None Text\
  \, theme =\
  \   \"blue\"\
  \, plugins =\
  \   [\"neuronignore\", \"dirtree\"] \
  \}"

-- Dhall's combine operator (`//`) allows us to merge two records,
-- effectively merging the record with defaults with the user record.
mergeWithDefault :: Text -> Text
mergeWithDefault userConfig =
  defaultConfig <> " // " <> userConfig
