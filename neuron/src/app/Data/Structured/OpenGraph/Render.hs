{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Structured.OpenGraph.Render where

import Data.Structured.OpenGraph
import Data.Time.ISO8601 (formatISO8601)
import Reflex.Dom.Core
import Relude
import qualified Text.URI as URI

renderOpenGraph :: forall t m. DomBuilder t m => OpenGraph -> m ()
renderOpenGraph OpenGraph {..} = do
  meta' "author" `mapM_` _openGraph_author
  meta' "description" `mapM_` _openGraph_description
  requireAbsolute "OGP URL" (\ourl -> elAttr "link" ("rel" =: "canonical" <> "href" =: ourl) blank) `mapM_` _openGraph_url
  metaOg "title" _openGraph_title
  metaOg "site_name" _openGraph_siteName
  whenJust _openGraph_type $ \case
    OGType_Article (Article {..}) -> do
      metaOg "type" "article"
      metaOg "article:section" `mapM_` _article_section
      metaOgTime "article:modified_time" `mapM_` _article_modifiedTime
      metaOgTime "article:published_time" `mapM_` _article_publishedTime
      metaOgTime "article:expiration_time" `mapM_` _article_expirationTime
      metaOg "article:tag" `mapM_` _article_tag
    OGType_Website -> do
      metaOg "type" "website"
  requireAbsolute "OGP image URL" (metaOg "image") `mapM_` _openGraph_image
  where
    meta' k v =
      elAttr "meta" ("name" =: k <> "content" =: v) blank
    metaOg k v =
      elAttr "meta" ("property" =: ("og:" <> k) <> "content" =: v) blank
    metaOgTime k t =
      metaOg k $ toText $ formatISO8601 t
    requireAbsolute :: Text -> (Text -> m ()) -> URI.URI -> m ()
    requireAbsolute description f uri' =
      if isJust (URI.uriScheme uri')
        then f $ URI.render uri'
        else error $ description <> " must be absolute. this URI is not: " <> URI.render uri'
