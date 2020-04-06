{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettel site's routes
module Neuron.Zettelkasten.Route where

import qualified Data.Text as T
import Neuron.Zettelkasten.Config
import Neuron.Zettelkasten.Graph
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Type
import Path
import Relude
import Rib (IsRoute (..))
import Rib.Extra.OpenGraph
import qualified Rib.Parser.MMark as MMark
import qualified Text.URI as URI

data Route store graph a where
  Route_IndexRedirect :: Route ZettelStore ZettelGraph ()
  Route_ZIndex :: Route ZettelStore ZettelGraph ()
  Route_Zettel :: ZettelID -> Route ZettelStore ZettelGraph ()

instance IsRoute (Route store graph) where
  routeFile = \case
    Route_IndexRedirect ->
      pure [relfile|index.html|]
    Route_ZIndex ->
      pure [relfile|z-index.html|]
    Route_Zettel (unZettelID -> zid) ->
      parseRelFile $ toString zid <> ".html"

-- | Return short name corresponding to the route
routeName :: Route store graph a -> Text
routeName = \case
  Route_IndexRedirect -> "Index"
  Route_ZIndex -> "Zettels"
  Route_Zettel zid -> unZettelID zid

-- | Return full title for a route
routeTitle :: Config -> store -> Route store graph a -> Text
routeTitle Config {..} store =
  withSuffix siteTitle . routeTitle' store
  where
    withSuffix suffix x =
      if x == suffix
        then x
        else x <> " - " <> suffix

-- | Return the title for a route
routeTitle' :: store -> Route store graph a -> Text
routeTitle' store = \case
  Route_IndexRedirect -> "Index"
  Route_ZIndex -> "Zettel Index"
  Route_Zettel (flip lookupStore store -> Zettel {..}) ->
    zettelTitle

routeOpenGraph :: Config -> store -> Route store graph a -> OpenGraph
routeOpenGraph Config {..} store r =
  OpenGraph
    { _openGraph_title = routeTitle' store r,
      _openGraph_siteName = siteTitle,
      _openGraph_description = case r of
        Route_IndexRedirect -> Nothing
        Route_ZIndex -> Just "Zettelkasten Index"
        Route_Zettel (flip lookupStore store -> Zettel {..}) ->
          T.take 300 <$> MMark.getFirstParagraphText zettelContent,
      _openGraph_author = author,
      _openGraph_type = case r of
        Route_Zettel _ -> Just $ OGType_Article (Article Nothing Nothing Nothing Nothing mempty)
        _ -> Just OGType_Website,
      _openGraph_image = case r of
        Route_Zettel (flip lookupStore store -> Zettel {..}) -> do
          img <- MMark.getFirstImg zettelContent
          baseUrl <- URI.mkURI =<< siteBaseUrl
          URI.relativeTo img baseUrl
        _ -> Nothing,
      _openGraph_url = Nothing
    }
