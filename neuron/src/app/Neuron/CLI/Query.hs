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
import Data.Some (withSome)
import Neuron.CLI.Logging (Message)
import Neuron.CLI.Types
import qualified Neuron.Cache as Cache
import qualified Neuron.Cache.Type as Cache
import qualified Neuron.Reactor as Reactor
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Query as Q
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
      let result = G.getZettel zid _neuronCache_graph
      putLTextLn $ Aeson.encodeToLazyText result
    CliQuery_Zettels -> do
      let result = G.getZettels _neuronCache_graph
      putLTextLn $ Aeson.encodeToLazyText result
    CliQuery_Graph someQ ->
      withSome someQ $ \q -> do
        let result = Q.runGraphQuery _neuronCache_graph q
        putLTextLn $ Aeson.encodeToLazyText $ Q.graphQueryResultJson q result _neuronCache_errors
