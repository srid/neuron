{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Static.StructuredData
  ( renderStructuredData,
  )
where

import Control.Monad.Except (liftEither, runExcept)
import Data.Structured.Breadcrumb (Breadcrumb)
import qualified Data.Structured.Breadcrumb as Breadcrumb
import Data.Structured.OpenGraph
  ( Article (Article),
    OGType (..),
    OpenGraph (..),
  )
import Data.Structured.OpenGraph.Render (renderOpenGraph)
import qualified Data.Text as T
import qualified Network.URI.Encode as E
import Neuron.Frontend.Route (Route (..))
import qualified Neuron.Frontend.Route as R
import qualified Neuron.Frontend.Route.Data.Types as R
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelT (..),
  )
import Reflex.Dom.Core (DomBuilder)
import Relude
import Text.Pandoc.Definition (Inline (Image), Pandoc (..))
import Text.Pandoc.Util (getFirstParagraphText, plainify)
import qualified Text.Pandoc.Walk as W
import Text.URI (URI, mkURI)
import qualified Text.URI as URI

renderStructuredData :: DomBuilder t m => Route a -> a -> m ()
renderStructuredData route val = do
  renderOpenGraph $ routeOpenGraph val route
  Breadcrumb.renderBreadcrumbs $ routeStructuredData val route

routeStructuredData :: a -> Route a -> [Breadcrumb]
routeStructuredData v = \case
  R.Route_Zettel _ ->
    case R.siteDataSiteBaseUrl (fst v) of
      Nothing -> []
      Just baseUrl ->
        let mkCrumb :: Zettel -> Breadcrumb.Item
            mkCrumb Zettel {..} =
              Breadcrumb.Item zettelTitle (Just $ routeUri baseUrl $ Route_Zettel zettelSlug)
         in Breadcrumb.fromForest $
              fmap mkCrumb <$> R.zettelDataUptree (snd v)
  _ ->
    []

routeOpenGraph :: a -> Route a -> OpenGraph
routeOpenGraph v r =
  OpenGraph
    { _openGraph_title = R.routeTitle' v r,
      _openGraph_siteName = R.siteDataSiteTitle (R.routeSiteData v r),
      _openGraph_description = case r of
        Route_Impulse -> Just "Impulse"
        Route_Zettel _slug -> do
          let zData = snd v
          doc <- getPandocDoc $ R.zettelDataZettel zData
          para <- getFirstParagraphText doc
          let paraText = plainify para
          pure $ T.take 300 paraText,
      _openGraph_author = R.siteDataSiteAuthor (R.routeSiteData v r),
      _openGraph_type = case r of
        Route_Zettel _ -> Just $ OGType_Article (Article Nothing Nothing Nothing Nothing mempty)
        _ -> Just OGType_Website,
      _openGraph_image = case r of
        Route_Zettel _ -> do
          doc <- getPandocDoc (R.zettelDataZettel $ snd v)
          image <- URI.mkURI =<< getFirstImg doc
          baseUrl <- R.siteDataSiteBaseUrl (fst v)
          URI.relativeTo image baseUrl
        _ -> Nothing,
      _openGraph_url = do
        baseUrl <- R.siteDataSiteBaseUrl (R.routeSiteData v r)
        pure $ routeUri baseUrl r
    }
  where
    getPandocDoc = either (const Nothing) (Just . zettelContent)
    getFirstImg ::
      Pandoc ->
      -- | Relative URL path to the image
      Maybe Text
    getFirstImg (Pandoc _ bs) = listToMaybe $
      flip W.query bs $ \case
        Image _ _ (url, _) -> [toText url]
        _ -> []

data BaseUrlError
  = BaseUrlNotAbsolute
  deriving (Eq, Show)

instance Exception BaseUrlError

-- | Make an absolute URI for a route, given a base URL.
routeUri :: HasCallStack => URI -> Route a -> URI
routeUri baseUrl r = either (error . toText . displayException) id $
  runExcept $ do
    let -- Use `E.encode` to deal with unicode code points, as mkURI will fail on them.
        -- This is necessary to support non-ascii characters in filenames
        relUrl = toText . E.encode . toString $ R.routeHtmlPath r
    uri <- liftEither $ mkURI relUrl
    case URI.relativeTo uri baseUrl of
      Nothing -> liftEither $ Left $ toException BaseUrlNotAbsolute
      Just x -> pure x
