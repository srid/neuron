{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Route.Data where

import qualified Clay.Render as C
import qualified Data.Dependent.Map as DMap
import Data.Foldable (Foldable (maximum))
import Data.TagTree (mkDefaultTagQuery, mkTagPattern)
import Data.Tree (Forest, Tree (..))
import Neuron.Cache.Type (NeuronCache (..))
import qualified Neuron.Config.Type as Config
import Neuron.Frontend.CSS (neuronStyleForTheme)
import Neuron.Frontend.Manifest (Manifest)
import Neuron.Frontend.Route (RouteConfig)
import Neuron.Frontend.Route.Data.Types
import qualified Neuron.Frontend.Theme as Theme
import qualified Neuron.Plugin as Plugin
import qualified Neuron.Plugin.Plugins.Tags as Tags
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (indexZid)
import Neuron.Zettelkasten.Zettel
import Relude hiding (traceShowId)

mkZettelData :: RouteConfig t m -> [ZettelC] -> NeuronCache -> SiteData -> ZettelC -> ZettelData
mkZettelData routeCfg zs NeuronCache {..} siteData zC = do
  let z = sansContent zC
      pluginData =
        DMap.fromList $
          Plugin.routePluginData routeCfg siteData zs neuroncacheGraph zC <$> maybe mempty DMap.toList (zettelPluginData z)
  ZettelData zC pluginData

mkImpulseData :: NeuronCache -> ImpulseData
mkImpulseData NeuronCache {..} =
  buildImpulse neuroncacheGraph neuroncacheErrors
  where
    buildImpulse graph errors =
      let (orphans, clusters) = partitionEithers $
            flip fmap (G.categoryClusters graph) $ \case
              [Node z []] -> Left z -- Orphans (cluster of exactly one)
              x -> Right x
          clustersWithUplinks :: [Forest (Zettel, [Zettel])] =
            -- Compute backlinks for each node in the tree.
            flip fmap clusters $ \(zs :: [Tree Zettel]) ->
              G.backlinksMulti Folgezettel zs graph
          stats = Stats (length $ G.getZettels graph) (G.connectionCount graph)
          pinnedZettels = Tags.zettelsByTag Tags.getZettelTags (G.getZettels graph) $ mkDefaultTagQuery [mkTagPattern "pinned"]
       in ImpulseData (fmap sortCluster clustersWithUplinks) orphans errors stats pinnedZettels
    -- TODO: Either optimize or get rid of this (or normalize the sorting somehow)
    sortCluster fs =
      sortZettelForest $
        flip fmap fs $ \Node {..} ->
          Node rootLabel $ sortZettelForest subForest
    -- Sort zettel trees so that trees containing the most recent zettel (by ID) come first.
    sortZettelForest = sortOn (Down . maximum)

mkSiteData :: NeuronCache -> HeadHtml -> Manifest -> SiteData
mkSiteData NeuronCache {..} headHtml manifest =
  let theme = Theme.mkTheme $ Config.theme neuroncacheConfig
      siteTitle = Config.siteTitle neuroncacheConfig
      siteAuthor = Config.author neuroncacheConfig
      baseUrl = join $ Config.getSiteBaseUrl neuroncacheConfig
      indexZettel = G.getZettel indexZid neuroncacheGraph
      editUrl = Config.editUrl neuroncacheConfig
      style = do
        neuronStyleForTheme theme
        Plugin.pluginStyles (Config.getPlugins neuroncacheConfig) theme
      bodyCss = toText $ C.renderWith C.compact [] style
   in SiteData
        theme
        siteTitle
        siteAuthor
        baseUrl
        editUrl
        bodyCss
        headHtml
        manifest
        neuroncacheNeuronVersion
        indexZettel
