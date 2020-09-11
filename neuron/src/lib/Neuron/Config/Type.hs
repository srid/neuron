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
    emptyConfig,
    mergeWithDefault,
    getZettelFormats,
  )
where

import Data.Aeson
import Neuron.Reader.Type (ZettelFormat)
import Relude hiding (readEither)
import Text.Read (readEither)

configFile :: FilePath
configFile = "neuron.dhall"

-- | Config type for @neuron.dhall@
--
-- See <https://neuron.zettel.page/2011701.html guide> for description of the fields.
--
-- TODO: Implement custom `FromDhall` instance, while using original field types
data Config = Config
  { aliases :: [Text],
    author :: Maybe Text,
    editUrl :: Maybe Text,
    mathJaxSupport :: Bool,
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

emptyConfig :: Text
emptyConfig = "{}"

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
  \, mathJaxSupport =\
  \   True\
  \, minVersion =\
  \   \"0.5\" \
  \}"

-- Dhall's combine operator (`//`) allows us to merge two records,
-- effectively merging the record with defaults with the user record.
mergeWithDefault :: Text -> Text
mergeWithDefault userConfig =
  defaultConfig <> " // " <> userConfig
