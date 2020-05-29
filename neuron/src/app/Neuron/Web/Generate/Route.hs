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
import Neuron.Web.Route (Route (..), RouteConfig (..))
import Neuron.Zettelkasten.ID
import Reflex.Dom.Core
import Relude
import Rib (IsRoute (..), routeUrl, routeUrlRel)
import qualified Text.URI as URI

instance IsRoute Route where
  routeFile = \case
    Route_Redirect zid ->
      routeFile $ Route_Zettel zid
    Route_ZIndex ->
      pure "z-index.html"
    Route_Search ->
      pure "search.html"
    Route_Zettel (zettelIDText -> s) ->
      pure $ toString s <> ".html"

staticRouteConfig :: RouteConfig t m
staticRouteConfig = RouteConfig True renderStaticRoute

renderStaticRoute :: DomBuilder t m => Some Route -> Map Text Text -> m a -> m a
renderStaticRoute someR attrs w =
  withSome someR $ \r -> do
    let hrefAttr :: Map Text Text = "href" =: routeUrlRel r
    elAttr "a" (attrs <> hrefAttr) w

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

data BaseUrlError
  = BaseUrlNotAbsolute
  deriving (Eq, Show)

instance Exception BaseUrlError

routeUri :: (HasCallStack, IsRoute r) => Text -> r a -> URI.URI
routeUri siteBaseUrl r = either (error . toText . displayException) id $ runExcept $ do
  baseUrl <- liftEither $ URI.mkURI siteBaseUrl
  uri <- liftEither $ URI.mkURI $ routeUrl r
  case URI.relativeTo uri baseUrl of
    Nothing -> liftEither $ Left $ toException BaseUrlNotAbsolute
    Just x -> pure x
