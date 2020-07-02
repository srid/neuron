{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Config.Type
  ( Config (..),
    configFile,
  )
where

import Dhall
import Neuron.Zettelkasten.Zettel.Format
import Relude hiding (bool, maybe)
import System.FilePattern

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
    formats :: [(FilePattern, ZettelFormat)],
    minVersion :: Text,
    siteBaseUrl :: Maybe Text,
    siteTitle :: Text,
    theme :: Text
  }

instance FromDhall Config where
  autoWith opts =
    record $
      Config <$> field "aliases" (list strictText)
        <*> field "author" (maybe strictText)
        <*> field "editUrl" (maybe strictText)
        <*> field "mathJaxSupport" bool
        <*> field "formats" (list formatRuleDecoder)
        <*> field "minVersion" strictText
        <*> field "siteBaseUrl" (maybe strictText)
        <*> field "siteTitle" strictText
        <*> field "theme" strictText
    where
      formatDecoder =
        union $
          (ZettelFormat_Markdown <$ constructor "Markdown" unit)
            <> (ZettelFormat_Org <$ constructor "Org" unit)
      formatRuleDecoder =
        record $
          (,) <$> (fmap toString $ field "pattern" strictText)
            <*> field "format" formatDecoder

deriving instance Generic Config
