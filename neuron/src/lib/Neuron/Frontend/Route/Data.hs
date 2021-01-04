{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Route.Data
  ( RouteDataCache,
    allSlugs,
    mkRouteDataCache,
    mkRouteData,
  )
where

import qualified Data.Map.Strict as Map
import Neuron.Cache.Type (NeuronCache (..))
import qualified Neuron.Config.Type as Config
import qualified Neuron.Frontend.Impulse as Impulse
import Neuron.Frontend.Route
import qualified Neuron.Frontend.Theme as Theme
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (Slug, indexZid)
import Neuron.Zettelkasten.Zettel (ZettelC, zettelSlug)
import Relude

-- This type is only used to store-once and retrieve-multiple-times the value
-- for Route_Zettel routes. Ideally, we need a better route system.
newtype RouteDataCache = RouteDataCache {unRouteDataCache :: Map Slug ZettelC}
  deriving (Eq, Show, Generic, Semigroup, Monoid)

allSlugs :: RouteDataCache -> [Slug]
allSlugs = Map.keys . unRouteDataCache

mkRouteDataCache :: [ZettelC] -> RouteDataCache
mkRouteDataCache zs =
  RouteDataCache $ Map.fromList $ zs <&> either zettelSlug zettelSlug &&& id

-- NOTE: RouteDataCache is used only for the Route_Zettel route.
mkRouteData :: RouteDataCache -> NeuronCache -> Route a -> a
mkRouteData (RouteDataCache slugMap) cache = \case
  Route_Impulse _ ->
    (mkSiteData cache, mkImpulseData cache)
  Route_ImpulseStatic ->
    (mkSiteData cache, mkImpulseData cache)
  Route_Zettel slug ->
    case Map.lookup slug slugMap of
      Just z ->
        (mkSiteData cache, (_neuronCache_graph cache, z))
      Nothing -> error "Impossible" -- HACK
  where
    mkImpulseData NeuronCache {..} =
      Impulse.buildImpulse _neuronCache_graph _neuronCache_errors
    mkSiteData NeuronCache {..} =
      let theme = Theme.mkTheme $ Config.theme _neuronCache_config
          indexZettel = G.getZettel indexZid _neuronCache_graph
          editUrl = Config.editUrl _neuronCache_config
       in SiteData theme _neuronCache_neuronVersion editUrl indexZettel
