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
  Route_Index :: Route ZettelStore ZettelGraph ()
  Route_Zettel :: ZettelID -> Route ZettelStore ZettelGraph ()

-- | Site properties
data Site
  = Site
      { -- | Title of the zettelkasten site
        siteTitle :: Text,
        siteAuthor :: Text,
        siteDescription :: Text,
        siteBaseUrl :: URI.URI
      }
  deriving (Eq, Show)

instance IsRoute (Route store graph) where
  routeFile = \case
    Route_Index ->
      pure [relfile|z-index.html|]
    Route_Zettel (unZettelID -> zid) ->
      parseRelFile $ toString zid <> ".html"

-- | Return short name corresponding to the route
routeName :: Route store graph a -> Text
routeName = \case
  Route_Index -> "Zettels"
  Route_Zettel zid -> unZettelID zid

-- | Return full title for a route
routeTitle :: Site -> store -> Route store graph a -> Text
routeTitle Site {..} store =
  withSuffix siteTitle . routeTitle' store
  where
    withSuffix suffix x =
      if x == suffix
        then x
        else x <> " - " <> suffix

-- | Return the title for a route
routeTitle' :: store -> Route store graph a -> Text
routeTitle' store = \case
  Route_Index -> "Zettel Index"
  Route_Zettel (flip lookupStore store -> Zettel {..}) ->
    zettelTitle

routeOpenGraph :: Site -> store -> Route store graph a -> OpenGraph
routeOpenGraph Site {..} store r =
  OpenGraph
    { _openGraph_title = routeTitle' store r,
      _openGraph_siteName = siteTitle,
      _openGraph_description = case r of
        Route_Index -> Just siteDescription
        Route_Zettel (flip lookupStore store -> Zettel {..}) ->
          T.take 300 <$> MMark.getFirstParagraphText zettelContent,
      _openGraph_author = siteAuthor,
      _openGraph_type = case r of
        Route_Index -> "website"
        Route_Zettel _ -> "article",
      _openGraph_image = case r of
        Route_Index -> Nothing
        Route_Zettel (flip lookupStore store -> Zettel {..}) ->
          flip URI.relativeTo siteBaseUrl =<< MMark.getFirstImg zettelContent
    }
