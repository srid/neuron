{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Query
  ( runQuery,
  )
where

import Colog (WithLog)
import qualified Data.Aeson.Text as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some (Some (Some), withSome)
import qualified Data.TagTree as TagTree
import Neuron.CLI.Logging (Message)
import Neuron.CLI.Types
import qualified Neuron.Cache as Cache
import qualified Neuron.Cache.Type as Cache
import qualified Neuron.Config.Type as Config
import qualified Neuron.Plugin.Plugins.Tags as Tags
import qualified Neuron.Reactor as Reactor
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Query as Q
import Neuron.Zettelkasten.Zettel (PluginZettelData (Tags))
import Relude

runQuery :: (MonadApp m, MonadFail m, MonadApp m, MonadIO m, WithLog env Message m) => QueryCommand -> m ()
runQuery QueryCommand {..} = do
  Cache.NeuronCache {..} <-
    fmap Cache.stripCache $
      if cached
        then Cache.getCache
        else do
          Reactor.loadZettelkasten >>= \case
            Left e -> fail $ toString e
            Right (ch, _, _) -> pure ch
  case query of
    CliQuery_ById zid -> do
      let result = G.getZettel zid neuroncacheGraph
      putLTextLn $ Aeson.encodeToLazyText result
    CliQuery_Zettels -> do
      let result = G.getZettels neuroncacheGraph
      putLTextLn $ Aeson.encodeToLazyText result
    CliQuery_Tags -> do
      let plugins = Config.getPlugins neuroncacheConfig
      if Some Tags `Map.member` plugins
        then do
          let result = Set.unions $ Tags.getZettelTags <$> G.getZettels neuroncacheGraph
          putLTextLn $ Aeson.encodeToLazyText result
        else fail "tags plugin is not enabled in neuron.dhall"
    CliQuery_ByTag tag -> do
      let plugins = Config.getPlugins neuroncacheConfig
      if Some Tags `Map.member` plugins
        then do
          let q = TagTree.mkDefaultTagQuery $ one $ TagTree.mkTagPatternFromTag tag
              zs = G.getZettels neuroncacheGraph
              result = Tags.zettelsByTag Tags.getZettelTags zs q
          putLTextLn $ Aeson.encodeToLazyText result
        else fail "tags plugin is not enabled in neuron.dhall"
    CliQuery_Graph someQ ->
      withSome someQ $ \q -> do
        result <- either (fail . show) pure $ Q.runGraphQuery neuroncacheGraph q
        putLTextLn $ Aeson.encodeToLazyText $ Q.graphQueryResultJson q result neuroncacheErrors
