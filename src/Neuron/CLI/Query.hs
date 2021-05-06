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
      printJson jsonl $ m2s result
    CliQuery_Zettels -> do
      let result = G.getZettels neuroncacheGraph
      printJson jsonl $ Set.fromList result
    CliQuery_Tags -> do
      let result = Set.unions $ Tags.getZettelTags <$> G.getZettels neuroncacheGraph
      printJson jsonl result
    CliQuery_ByTag tag -> do
      let q = TagTree.mkDefaultTagQuery $ one $ TagTree.mkTagPatternFromTag tag
          zs = G.getZettels neuroncacheGraph
          result = Tags.zettelsByTag Tags.getZettelTags zs q
      printJson jsonl $ Set.fromList result
    CliQuery_Graph someQ ->
      withSome someQ $ \q -> do
        result <- either (fail . show) pure $ Q.runGraphQuery neuroncacheGraph q
        printJson jsonl $ Set.singleton $ Q.graphQueryResultJson q result neuroncacheErrors
  where
    m2s :: Maybe a -> Set a
    m2s Nothing = Set.empty
    m2s (Just a) = Set.singleton a
    printJson :: ToJSON a => Bool -> Set a -> m ()
    printJson True = printJsonLine
    printJson False = printPrettyJson
    printJsonLine :: ToJSON a => Set a -> m ()
    printJsonLine = mapM_ (putLBSLn . Aeson.encode)
    printPrettyJson :: ToJSON a => Set a -> m ()
    printPrettyJson =
      putTextLn . decodeUtf8
        . AesonPretty.encodePretty'
          AesonPretty.defConfig
            { -- Sort hash map by keys for consistency
              AesonPretty.confCompare = compare
            }
