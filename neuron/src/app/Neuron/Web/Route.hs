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

-- | Zettel site's routes
module Neuron.Web.Route where

import Control.Monad.Except
import qualified Data.Text as T
import GHC.Stack
import Neuron.Config
import Neuron.Markdown (getFirstParagraphText)
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Zettel
import Relude
import Rib (IsRoute (..), routeUrl, routeUrlRel)
import Rib.Extra.OpenGraph
import qualified Rib.Parser.Pandoc as Pandoc
import Text.Pandoc (def, runPure, writePlain)
import Text.Pandoc.Definition (Block (Plain), Inline, Pandoc (..))
import qualified Text.URI as URI

data Route graph a where
  Route_Redirect :: ZettelID -> Route ZettelGraph ZettelID
  -- ZIndex takes a report of all errors in the zettelkasten.
  -- `Left` is skipped zettels; and Right is valid zettels with invalid query links.
  Route_ZIndex :: Route ZettelGraph (Map ZettelID (Either Text [QueryError]))
  Route_Search :: Route ZettelGraph ()
  Route_Zettel :: ZettelID -> Route ZettelGraph PandocZettel

type family RouteError r

type instance RouteError (Map ZettelID (Either Text [QueryError])) = ()

type instance RouteError ZettelID = ()

type instance RouteError () = ()

type instance RouteError PandocZettel = [QueryError]

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

routeUri :: (HasCallStack, IsRoute r) => Text -> r a -> URI.URI
routeUri siteBaseUrl r = either (error . toText . displayException) id $ runExcept $ do
  baseUrl <- liftEither $ URI.mkURI siteBaseUrl
  uri <- liftEither $ URI.mkURI $ routeUrl r
  case URI.relativeTo uri baseUrl of
    Nothing -> liftEither $ Left $ toException BaseUrlNotAbsolute
    Just x -> pure x

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
    let PandocZettel (z, _) = val
     in zettelTitle z

routeOpenGraph :: Config -> a -> Route graph a -> OpenGraph
routeOpenGraph Config {..} val r =
  OpenGraph
    { _openGraph_title = routeTitle' val r,
      _openGraph_siteName = siteTitle,
      _openGraph_description = case r of
        Route_Redirect _ -> Nothing
        Route_ZIndex -> Just "Zettelkasten Index"
        Route_Search -> Just "Search Zettelkasten"
        Route_Zettel _ -> do
          let PandocZettel (_, doc) = val
          para <- getFirstParagraphText doc
          paraText <- renderPandocAsText para
          pure $ T.take 300 paraText,
      _openGraph_author = author,
      _openGraph_type = case r of
        Route_Zettel _ -> Just $ OGType_Article (Article Nothing Nothing Nothing Nothing mempty)
        _ -> Just OGType_Website,
      _openGraph_image = case r of
        Route_Zettel _ -> do
          let PandocZettel (_, doc) = val
          img <- URI.mkURI =<< Pandoc.getFirstImg doc
          baseUrl <- URI.mkURI =<< siteBaseUrl
          URI.relativeTo img baseUrl
        _ -> Nothing,
      _openGraph_url = do
        baseUrl <- siteBaseUrl
        pure $ routeUri baseUrl r
    }
  where
    renderPandocAsText :: [Inline] -> Maybe Text
    renderPandocAsText =
      either (const Nothing) Just . runPure . writePlain def . Pandoc mempty . pure . Plain
