{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Neuron's route and its config
module Neuron.Frontend.Route where

import Data.Constraint.Extras.TH (deriveArgDict)
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import Control.Monad.Except (liftEither, runExcept)
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Some (Some, foldSome)
import qualified Data.Text as T
import qualified Network.URI.Encode as E
import Neuron.Frontend.Route.Data.Types
  ( ImpulseData,
    SiteData,
    ZettelData (zettelDataZettel),
  )
import Neuron.Zettelkasten.ID (Slug)
import Neuron.Zettelkasten.Zettel
  ( ZettelT (zettelTitle),
  )
import Reflex.Dom.Core
import Relude
import Text.URI (URI)
import qualified Text.URI as URI

data Route routeData where
  Route_Zettel :: Slug -> Route (SiteData, ZettelData)
  Route_Impulse :: Route (SiteData, ImpulseData)

routeSiteData :: a -> Route a -> SiteData
routeSiteData val = \case
  Route_Zettel _ -> fst val
  Route_Impulse -> fst val

routeHtmlPath :: Route a -> FilePath
routeHtmlPath = \case
  Route_Impulse ->
    "impulse.html"
  Route_Zettel slug ->
    toString slug <> ".html"

data RouteConfig t m = RouteConfig
  { -- | How to render a web route.
    routeConfigRouteLink :: (DomBuilder t m, PostBuild t m) => Dynamic t (Some Route) -> Map Text Text -> m () -> m (),
    -- | Get the URL for a web route as plain text
    routeConfigRouteURL :: Some Route -> Text
  }

mkRouteUrl :: Route a -> Text
mkRouteUrl =
  toText . obviateIndexHtml . routeHtmlPath
  where
    obviateIndexHtml = \case
      "index.html" -> "."
      x -> x

-- | Like `mkRouteUrl` but drops the .html at the end
mkPrettyRouteUrl :: Route a -> Text
mkPrettyRouteUrl =
  dropHtmlExt . mkRouteUrl
  where
    dropHtmlExt (T.stripSuffix ".html" -> Just url) = url
    dropHtmlExt url = url

mkRouteConfig :: (forall a. Route a -> Text) -> RouteConfig t m
mkRouteConfig mkUrl =
  RouteConfig renderRouteLink someRouteUrl
  where
    renderRouteLink dynR attrs =
      elDynAttr "a" $ ffor dynR $ \someR -> attrs <> "href" =: someRouteUrl someR
    someRouteUrl :: Some Route -> Text
    someRouteUrl = foldSome mkUrl

type NeuronWebT t m = ReaderT (RouteConfig t m) m

runNeuronWeb :: RouteConfig t m -> NeuronWebT t m a -> m a
runNeuronWeb = flip runReaderT

neuronRouteLink :: (DomBuilder t m, PostBuild t m) => Some Route -> Map Text Text -> m () -> NeuronWebT t m ()
neuronRouteLink someR =
  neuronDynRouteLink (constDyn someR)

neuronDynRouteLink ::
  (DomBuilder t m, PostBuild t m) => Dynamic t (Some Route) -> Map Text Text -> m () -> NeuronWebT t m ()
neuronDynRouteLink rDyn attrs w = do
  f <- asks routeConfigRouteLink
  lift $ f rDyn attrs w

neuronRouteURL :: Monad m => Some Route -> NeuronWebT t m Text
neuronRouteURL someR = do
  f <- asks routeConfigRouteURL
  pure $ f someR

routeTitle' :: a -> Route a -> Text
routeTitle' v = \case
  Route_Impulse -> "Impulse"
  Route_Zettel _ ->
    either zettelTitle zettelTitle $ zettelDataZettel . snd $ v

data BaseUrlError
  = BaseUrlNotAbsolute
  deriving (Eq, Show)

instance Exception BaseUrlError

-- | Make an absolute URI for the given relative URL, given a base URL.
routeUri :: HasCallStack => URI -> Text -> URI
routeUri baseUrl relUrl = either (error . toText . displayException) id $
  runExcept $ do
    let -- Use `E.encode` to deal with unicode code points, as mkURI will fail on them.
        -- This is necessary to support non-ascii characters in filenames
        relUrlEncoded = toText . E.encode . toString $ relUrl
    uri <- liftEither $ URI.mkURI relUrlEncoded
    case URI.relativeTo uri baseUrl of
      Nothing -> liftEither $ Left $ toException BaseUrlNotAbsolute
      Just x -> pure x

deriveGEq ''Route
deriveGShow ''Route
deriveGCompare ''Route
deriveArgDict ''Route
