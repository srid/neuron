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
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Tagged (Tagged)
import Data.Tree (Forest)
import Neuron.Frontend.Manifest (Manifest)
import Neuron.Frontend.Theme (Theme)
import Neuron.Zettelkasten.Connection (ContextualConnection)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Query.Eval (QueryUrlCache)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelC,
  )
import Neuron.Zettelkasten.Zettel.Error (ZettelIssue)
import Relude
import Text.URI (URI)

type NeuronVersion = Tagged "NeuronVersion" Text

newtype HeadHtml = HeadHtml (Maybe Text)
  deriving (Eq)

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
    -- Data from filesystem
    siteDataHeadHtml :: HeadHtml,
    siteDataManifest :: Manifest,
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

data Stats = Stats
  { statsZettelCount :: Int,
    statsZettelConnectionCount :: Int
  }
  deriving (Eq, Show)

-- Plugin types

data DirZettelVal = DirZettelVal
  { dirZettelValChildren :: [Zettel],
    dirZettelValParent :: Maybe Zettel
  }
  deriving (Eq, Show)

instance Default DirZettelVal where
  def = DirZettelVal mempty Nothing

data PluginZettelRouteData routeData where
  PluginZettelRouteData_DirTree :: PluginZettelRouteData DirZettelVal
  PluginZettelRouteData_NeuronIgnore :: PluginZettelRouteData ()

deriveArgDict ''PluginZettelRouteData
deriveJSONGADT ''PluginZettelRouteData
deriveGEq ''PluginZettelRouteData
deriveGShow ''PluginZettelRouteData
deriveGCompare ''PluginZettelRouteData

deriving instance Eq ZettelData