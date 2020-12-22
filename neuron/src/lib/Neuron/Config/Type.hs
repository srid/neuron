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
  )
where

import Data.Aeson
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
    minVersion :: Text,
    siteBaseUrl :: Maybe Text,
    siteTitle :: Text,
    theme :: Text,
    -- EXPERIMENTAL: may go way, or be changed
    recurseDir :: Bool
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
  \, recurseDir =\
  \   False \
  \, minVersion =\
  \   \"0.5\" \
  \}"

-- Dhall's combine operator (`//`) allows us to merge two records,
-- effectively merging the record with defaults with the user record.
mergeWithDefault :: Text -> Text
mergeWithDefault userConfig =
  defaultConfig <> " // " <> userConfig
