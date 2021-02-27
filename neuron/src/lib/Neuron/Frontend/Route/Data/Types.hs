{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Neuron's route and its config
module Neuron.Frontend.Route.Data.Types where

import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Default (Default (..))
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Tagged (Tagged)
import Data.Time.DateMayTime (DateMayTime)
import Data.Tree (Forest)
import Neuron.Frontend.Manifest (Manifest)
import Neuron.Frontend.Theme (Theme)
import Neuron.Zettelkasten.Connection (Connection, ContextualConnection)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.Zettel.Error (ZettelIssue)
import Relude
import Text.Pandoc.Definition (Pandoc)
import Text.URI (URI)

type NeuronVersion = Tagged "NeuronVersion" Text

newtype HeadHtml = HeadHtml (Maybe Text)
  deriving (Eq, Show)

instance Default HeadHtml where
  def = HeadHtml Nothing

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
    siteDataBodyCss :: Text,
    -- Data from filesystem
    siteDataHeadHtml :: HeadHtml,
    siteDataManifest :: Manifest,
    -- Neuron's version
    siteDataNeuronVersion :: NeuronVersion,
    -- Reference to `index.md` zettel if any.
    siteDataIndexZettel :: Maybe Zettel
  }
  deriving (Eq, Show)

data ZettelData = ZettelData
  { zettelDataZettel :: ZettelC,
    zettelDataPlugin :: DMap PluginZettelRouteData Identity
  }

-- | The value needed to render the Impulse page
--
-- All heavy graph computations are decoupled from rendering, producing this
-- value, that is in turn used for instant rendering.
-- TODO: rename to `ImpulseData` for consistency
data ImpulseData = ImpulseData
  { -- | Clusters on the folgezettel graph.
    impulseDataClusters :: [Forest (Zettel, [Zettel])],
    impulseDataOrphans :: [Zettel],
    -- | All zettel errors
    impulseDataErrors :: Map ZettelID ZettelIssue,
    impulseDataStats :: Stats,
    impulseDataPinned :: [Zettel]
  }
  deriving (Eq, Show)

data Stats = Stats
  { statsZettelCount :: Int,
    statsZettelConnectionCount :: Int
  }
  deriving (Eq, Show)

-- Plugin types for route data

data DirZettelVal = DirZettelVal
  { dirZettelValChildren :: [(ContextualConnection, Zettel)],
    dirZettelValParent :: Maybe Zettel,
    dirZettelValMeta :: DirTreeMeta
  }
  deriving (Eq, Show)

instance Default DirZettelVal where
  def = DirZettelVal mempty Nothing def

data LinksData = LinksData
  { linksDataLinkCache :: Map Text (Either MissingZettel (Connection, Zettel)),
    linksDataBacklinks :: [(ContextualConnection, Zettel)]
  }
  deriving (Eq, Show)

instance Default LinksData where
  def = LinksData mempty mempty

type TagQueryCache = Map Text (DSum TagQuery Identity)

data FeedData = FeedData
  { feedDataTitle :: Text,
    feedDataSlug :: Slug,
    feedDataBaseUri :: URI,
    feedDataUrl :: URI,
    feedDataEntries :: [FeedItem]
  }
  deriving (Eq, Show)

data FeedItem = FeedItem
  { feedItemSlug :: Slug,
    feedItemTitle :: Text,
    feedItemAuthor :: Maybe Text,
    feedItemDate :: DateMayTime,
    feedItemZettelID :: ZettelID,
    feedItemZettelContent :: Pandoc
  }
  deriving (Eq, Show)

data PluginZettelRouteData routeData where
  PluginZettelRouteData_DirTree :: PluginZettelRouteData DirZettelVal
  PluginZettelRouteData_Links :: PluginZettelRouteData LinksData
  PluginZettelRouteData_Tags :: PluginZettelRouteData TagQueryCache
  PluginZettelRouteData_NeuronIgnore :: PluginZettelRouteData ()
  PluginZettelRouteData_UpTree :: PluginZettelRouteData (Forest Zettel)
  PluginZettelRouteData_Feed :: PluginZettelRouteData FeedData

deriveArgDict ''PluginZettelRouteData
deriveJSONGADT ''PluginZettelRouteData
deriveGEq ''PluginZettelRouteData
deriveGShow ''PluginZettelRouteData
deriveGCompare ''PluginZettelRouteData

deriving instance Eq ZettelData
