{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | JSON for Google's structured data breadcrumbs
--
-- https://developers.google.com/search/docs/data-types/breadcrumb
module Data.Structured.Breadcrumb where

import Data.Aeson
import Data.Tree
import Reflex.Dom.Core hiding (mapMaybe)
import Relude
import Text.URI (URI)
import qualified Text.URI as URI

data Item = Item
  { name :: Text,
    url :: Maybe URI
  }
  deriving (Eq, Show, Generic)

newtype Breadcrumb = Breadcrumb {unBreadcrumb :: NonEmpty Item}
  deriving (Eq, Show, Generic)

renderBreadcrumbs :: DomBuilder t m => [Breadcrumb] -> m ()
renderBreadcrumbs bs =
  elAttr "script" ("type" =: "application/ld+json") $ text $ decodeUtf8 $ encode bs

instance ToJSON Breadcrumb where
  toJSON (Breadcrumb (toList -> crumbs)) =
    toJSON $
      object
        [ "@context" .= toJSON context,
          "@type" .= ("BreadcrumbList" :: Text),
          "itemListElement" .= toJSON (uncurry itemJson <$> zip [1 :: Int ..] crumbs)
        ]
    where
      context = "https://schema.org" :: Text
      itemJson pos Item {..} =
        object
          [ "@type" .= toJSON ("ListItem" :: Text),
            "position" .= toJSON pos,
            "name" .= toJSON name,
            "item" .= toJSON (fmap URI.render url)
          ]

fromForest :: Forest Item -> [Breadcrumb]
fromForest =
  fmap Breadcrumb . mapMaybe (nonEmpty . reverse) . concatMap (foldTree f)
  where
    f :: Item -> [[[Item]]] -> [[Item]]
    f parent = \case
      [] -> [[parent]]
      childPaths ->
        fmap (parent :) `concatMap` childPaths
{-
    [{
      "@context": "https://schema.org",
      "@type": "BreadcrumbList",
      "itemListElement": [{
        "@type": "ListItem",
        "position": 1,
        "name": "Books",
        "item": "https://example.com/books"
      },{
        "@type": "ListItem",
        "position": 2,
        "name": "Science Fiction",
        "item": "https://example.com/books/sciencefiction"
      },{
        "@type": "ListItem",
        "position": 3,
        "name": "Award Winners"
      }]
    },
    {
      "@context": "https://schema.org",
      "@type": "BreadcrumbList",
      "itemListElement": [{
        "@type": "ListItem",
        "position": 1,
        "name": "Literature",
        "item": "https://example.com/literature"
      },{
        "@type": "ListItem",
        "position": 2,
        "name": "Award Winners"
      }]
    }]
-}
