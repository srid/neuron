{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Static generation of routes
module Neuron.Web.Generate.Route where

import Control.Monad.Except
import Data.Some
import Data.TagTree (unTag)
import Neuron.Web.Route (Route (..), RouteConfig (..))
import Neuron.Zettelkasten.ID
import Reflex.Dom.Core
import Relude
import Rib.Route (IsRoute (..), routeUrl, routeUrlRel)
import qualified Text.URI as URI

instance IsRoute Route where
  routeFile = \case
    Route_Redirect zid ->
      routeFile $ Route_Zettel zid
    Route_ZIndex ->
      pure "z-index.html"
    Route_Search _mtag ->
      pure "search.html"
    Route_Zettel (zettelIDText -> s) ->
      pure $ toString s <> ".html"

staticRouteConfig :: RouteConfig t m
staticRouteConfig =
  RouteConfig True renderStaticRoute staticRouteUrl
  where
    renderStaticRoute :: DomBuilder t m => Some Route -> Map Text Text -> m a -> m a
    renderStaticRoute someR attrs w =
      withSome someR $ \r -> do
        let hrefAttr :: Map Text Text = "href" =: routeFor r
        elAttr "a" (attrs <> hrefAttr) w
    staticRouteUrl someR =
      withSome someR $ \r -> do
        routeFor r
    -- Using relative URLs enables the site work in file:/// URLs
    routeFor = \case
      -- HACK: Hack around Rib.Route's limitation in dealing with query arguments
      r@(Route_Search (Just t)) -> routeUrlRel r <> "?tag=" <> unTag t
      r -> routeUrlRel r

data BaseUrlError
  = BaseUrlNotAbsolute
  deriving (Eq, Show)

instance Exception BaseUrlError

-- | Make an absolute URI for a route, given a base URL.
routeUri :: (HasCallStack, IsRoute r) => Text -> r a -> URI.URI
routeUri siteBaseUrl r = either (error . toText . displayException) id $ runExcept $ do
  baseUrl <- liftEither $ URI.mkURI siteBaseUrl
  uri <- liftEither $ URI.mkURI $ routeUrl r
  case URI.relativeTo uri baseUrl of
    Nothing -> liftEither $ Left $ toException BaseUrlNotAbsolute
    Just x -> pure x
