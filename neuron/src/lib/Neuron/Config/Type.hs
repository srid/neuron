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

import Relude

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
    minVersion :: Text,
    siteBaseUrl :: Maybe Text,
    siteTitle :: Text,
    theme :: Text
  }

deriving instance Generic Config
