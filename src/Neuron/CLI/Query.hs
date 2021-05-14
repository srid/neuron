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
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Set as Set
import Data.Some (withSome)
import qualified Data.TagTree as TagTree
import Neuron.CLI.Logging (Message)
import Neuron.CLI.Types
import qualified Neuron.Cache as Cache
import qualified Neuron.Cache.Type as Cache
import qualified Neuron.Plugin.Plugins.Tags as Tags
import qualified Neuron.Reactor as Reactor
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Query as Q
import Relude

runQuery :: forall m env. (MonadApp m, MonadFail m, MonadApp m, MonadIO m, WithLog env Message m) => QueryCommand -> m ()
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
      bool printPrettyJson (printJsonLine . maybeToList) jsonl result
    CliQuery_Zettels -> do
      let result = G.getZettels neuroncacheGraph
      bool printPrettyJson printJsonLine jsonl result
    CliQuery_Tags -> do
      let result = Set.unions $ Tags.getZettelTags <$> G.getZettels neuroncacheGraph
      bool printPrettyJson (printJsonLine . Set.toList) jsonl result
    CliQuery_ByTag tag -> do
      let q = TagTree.mkDefaultTagQuery $ one $ TagTree.mkTagPatternFromTag tag
          zs = G.getZettels neuroncacheGraph
          result = Tags.zettelsByTag Tags.getZettelTags zs q
      bool printPrettyJson printJsonLine jsonl result
    CliQuery_Graph someQ ->
      withSome someQ $ \q -> do
        result <- either (fail . show) pure $ Q.runGraphQuery neuroncacheGraph q
        bool printPrettyJson printJsonLine jsonl [Q.graphQueryResultJson q result neuroncacheErrors]
  where
    printJsonLine :: ToJSON a => [a] -> m ()
    printJsonLine = mapM_ (putLBSLn . Aeson.encode)
    printPrettyJson :: ToJSON a => a -> m ()
    printPrettyJson =
      putLBSLn
        . AesonPretty.encodePretty'
          AesonPretty.defConfig
            { -- Sort hash map by keys for consistency
              AesonPretty.confCompare = compare
            }
