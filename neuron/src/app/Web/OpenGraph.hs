{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Meta tags for The Open Graph protocol: https://ogp.me/
module Web.OpenGraph
  ( OpenGraph (..),
    OGType (..),
    Article (..),
  )
where

import Data.Time (UTCTime)
import Relude
import qualified Text.URI as URI

-- The OpenGraph metadata
--
-- This type can be directly rendered to HTML using `toHTML`.
data OpenGraph = OpenGraph
  { _openGraph_title :: Text,
    _openGraph_url :: Maybe URI.URI,
    _openGraph_author :: Maybe Text,
    _openGraph_description :: Maybe Text,
    _openGraph_siteName :: Text,
    _openGraph_type :: Maybe OGType,
    _openGraph_image :: Maybe URI.URI
  }
  deriving (Eq, Show)

-- TODO: Remaining ADT values & sub-fields
data OGType
  = OGType_Article Article
  | OGType_Website
  deriving (Eq, Show)

-- TODO: _article_profile :: [Profile]
data Article = Article
  { _article_section :: Maybe Text,
    _article_modifiedTime :: Maybe UTCTime,
    _article_publishedTime :: Maybe UTCTime,
    _article_expirationTime :: Maybe UTCTime,
    _article_tag :: [Text]
  }
  deriving (Eq, Show)
