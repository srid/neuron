{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettel site's routes
module Neuron.Web.Route where

import qualified Data.Text as T
import GHC.Stack
import Neuron.Config
import Neuron.Zettelkasten.Graph
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link
import Neuron.Zettelkasten.Zettel
import Relude
import Rib (IsRoute (..), routeUrlRel)
import Rib.Extra.OpenGraph
import qualified Rib.Parser.MMark as MMark
import qualified Text.URI as URI

data Route graph a where
  Route_Redirect :: ZettelID -> Route ZettelGraph ZettelID
  Route_ZIndex :: Route ZettelGraph ()
  Route_Search :: Route ZettelGraph ()
  Route_Zettel :: ZettelID -> Route ZettelGraph (Zettel, ZettelQueryResource)

instance IsRoute (Route graph) where
  routeFile = \case
    Route_Redirect zid ->
      routeFile $ Route_Zettel zid
    Route_ZIndex ->
      pure "z-index.html"
    Route_Search ->
      pure "search.html"
    Route_Zettel (zettelIDText -> s) ->
      pure $ toString s <> ".html"

-- | Like `routeUrlRel` but takes a query parameter
routeUrlRelWithQuery :: HasCallStack => IsRoute r => r a -> URI.RText 'URI.QueryKey -> Text -> Text
routeUrlRelWithQuery r k v = maybe (error "Bad URI") URI.render $ do
  param <- URI.QueryParam k <$> URI.mkQueryValue v
  route <- URI.mkPathPiece $ routeUrlRel r
  pure
    URI.emptyURI
      { URI.uriPath = Just (False, route :| []),
        URI.uriQuery = [param]
      }

-- | Return full title for a route
routeTitle :: Config -> a -> Route graph a -> Text
routeTitle Config {..} val =
  withSuffix siteTitle . routeTitle' val
  where
    withSuffix suffix x =
      if x == suffix
        then x
        else x <> " - " <> suffix

-- | Return the title for a route
routeTitle' :: a -> Route graph a -> Text
routeTitle' val = \case
  Route_Redirect _ -> "Redirecting..."
  Route_ZIndex -> "Zettel Index"
  Route_Search -> "Search"
  Route_Zettel _ ->
    zettelTitle $ fst val

routeOpenGraph :: Config -> a -> Route graph a -> OpenGraph
routeOpenGraph Config {..} val r =
  OpenGraph
    { _openGraph_title = routeTitle' val r,
      _openGraph_siteName = siteTitle,
      _openGraph_description = case r of
        Route_Redirect _ -> Nothing
        Route_ZIndex -> Just "Zettelkasten Index"
        Route_Search -> Just "Search Zettelkasten"
        Route_Zettel _ ->
          T.take 300 <$> MMark.getFirstParagraphText (zettelContent $ fst val),
      _openGraph_author = author,
      _openGraph_type = case r of
        Route_Zettel _ -> Just $ OGType_Article (Article Nothing Nothing Nothing Nothing mempty)
        _ -> Just OGType_Website,
      _openGraph_image = case r of
        Route_Zettel _ -> do
          img <- MMark.getFirstImg (zettelContent $ fst val)
          baseUrl <- URI.mkURI =<< siteBaseUrl
          URI.relativeTo img baseUrl
        _ -> Nothing,
      _openGraph_url = Nothing
    }
