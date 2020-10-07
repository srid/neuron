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
    getZettelFormats,
    getSiteBaseUrl,
  )
where

import Data.Aeson
import Neuron.Reader.Type (ZettelFormat)
import Relude hiding (readEither)
import Text.Read (readEither)
import Text.URI (URI, mkURI)

configFile :: FilePath
configFile = "neuron.dhall"

-- | Config type for @neuron.dhall@
--
-- See <https://neuron.zettel.page/configuration.html guide> for description of the fields.
--
-- TODO: Implement custom `FromDhall` instance, while using original field types
data Config = Config
  { aliases :: [Text],
    author :: Maybe Text,
    editUrl :: Maybe Text,
    -- TODO: This should use `NonEmpty`.
    formats :: [Text],
    minVersion :: Text,
    siteBaseUrl :: Maybe Text,
    siteTitle :: Text,
    theme :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

getZettelFormats :: MonadFail m => Config -> m (NonEmpty ZettelFormat)
getZettelFormats Config {..} = do
  formats' <- maybe (fail "Empty formats") pure $ nonEmpty formats
  traverse (either (fail . toString) pure . readEither . toString) formats'

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
  \, aliases =\
  \   [] : List Text\
  \, formats =\
  \   [ \"markdown\" ]\
  \, minVersion =\
  \   \"0.5\" \
  \}"

-- Dhall's combine operator (`//`) allows us to merge two records,
-- effectively merging the record with defaults with the user record.
mergeWithDefault :: Text -> Text
mergeWithDefault userConfig =
  defaultConfig <> " // " <> userConfig
