{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Neuron's route and its config
module Neuron.Frontend.Route where

import Data.GADT.Compare.TH (DeriveGEQ (deriveGEq))
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Some (Some, withSome)
import Data.TagTree (Tag, unTag)
import Data.Tagged (Tagged)
import Data.Tree (Forest)
import Neuron.Frontend.Theme (Theme)
import Neuron.Zettelkasten.Connection (ContextualConnection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Query.Eval (QueryUrlCache)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelC,
    ZettelT (zettelTitle),
  )
import Neuron.Zettelkasten.Zettel.Error (ZettelIssue)
import Reflex.Dom.Core
import Relude
import Text.URI (URI)

type NeuronVersion = Tagged "NeuronVersion" Text

-- | TODO: Pull SiteData out of GADT, as it exists in all constructors.
data Route a where
  -- TODO: Eliminate ZettelGraph by taking just necessary subset into route data
  Route_Zettel :: Slug -> Route (SiteData, ZettelData)
  -- | Impulse is implemented in github.com/srid/rememorate
  -- The tag argument is only used in rendering the URL, and not when writing the file.
  -- TODO: Fix this bad use of types.
  Route_Impulse :: Maybe Tag -> Route (SiteData, Impulse)
  Route_ImpulseStatic :: Route (SiteData, Impulse)

routeSiteData :: a -> Route a -> SiteData
routeSiteData val = \case
  Route_Zettel _ -> fst val
  Route_Impulse _ -> fst val
  Route_ImpulseStatic -> fst val

-- | Site-wide data common to all routes.
--
-- Significance: changes to these data must regenerate the routes, even if the
-- route-specific data hasn't changed.
data SiteData = SiteData
  { siteDataTheme :: Theme,
    -- Config from neuron.dhall
    siteDataSiteTitle :: Text,
    siteDataSiteAuthor :: Maybe Text,
    siteDataSiteBaseUrl :: Maybe URI,
    siteDataEditUrl :: Maybe Text,
    -- Neuron's version
    siteDataNeuronVersion :: NeuronVersion,
    -- Reference to `index.md` zettel if any.
    siteDataIndexZettel :: Maybe Zettel
  }
  deriving (Eq)

data ZettelData = ZettelData
  { zettelDataZettel :: ZettelC,
    zettelDataQueryUrlCache :: QueryUrlCache,
    zettelDataUptree :: Forest Zettel,
    zettelDataBacklinks :: [(ContextualConnection, Zettel)],
    -- TODO: eliminate this
    zettelDataGraph :: ZettelGraph
  }
  deriving (Eq)

-- | The value needed to render the Impulse page
--
-- All heavy graph computations are decoupled from rendering, producing this
-- value, that is in turn used for instant rendering.
-- TODO: rename to `ImpulseData` for consistency
data Impulse = Impulse
  { -- | Clusters on the folgezettel graph.
    impulseClusters :: [Forest (Zettel, [Zettel])],
    impulseOrphans :: [Zettel],
    -- | All zettel errors
    impulseErrors :: Map ZettelID ZettelIssue,
    impulseStats :: Stats,
    impulsePinned :: [Zettel]
  }

data Stats = Stats
  { statsZettelCount :: Int,
    statsZettelConnectionCount :: Int
  }
  deriving (Eq, Show)

routeHtmlPath :: Route a -> FilePath
routeHtmlPath = \case
  Route_Impulse (Just t) ->
    "impulse.html?q=tag:" <> toString (unTag t)
  Route_Impulse Nothing ->
    "impulse.html"
  Route_ImpulseStatic ->
    "impulse-static.html"
  Route_Zettel slug ->
    toString slug <> ".html"

data RouteConfig t m = RouteConfig
  { -- | How to render a web route.
    routeConfigRouteLink :: (DomBuilder t m, PostBuild t m) => Dynamic t (Some Route) -> Map Text Text -> m () -> m (),
    -- | Get the URL for a web route as plain text
    routeConfigRouteURL :: Some Route -> Text
  }

routeConfig :: RouteConfig t m
routeConfig =
  RouteConfig renderRouteLink someRouteUrl
  where
    renderRouteLink dynR attrs =
      elDynAttr "a" $ ffor dynR $ \someR -> attrs <> "href" =: someRouteUrl someR
    someRouteUrl :: Some Route -> Text
    someRouteUrl sr =
      toText $ withSome sr $ obviateIndexHtml . routeHtmlPath
    obviateIndexHtml = \case
      "index.html" -> "."
      x -> x

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
  Route_Impulse _mtag -> "Impulse"
  Route_ImpulseStatic -> "Impulse (static)"
  Route_Zettel _ ->
    either zettelTitle zettelTitle $ zettelDataZettel . snd $ v

deriveGEq ''Route

deriveGShow ''Route
