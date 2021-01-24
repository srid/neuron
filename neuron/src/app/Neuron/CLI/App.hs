{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.App
  ( run,
  )
where

import Control.Concurrent.Async (race_)
import qualified Data.Aeson.Text as Aeson
import Data.Some (withSome)
import Data.Tagged
import Data.Time
  ( getCurrentTime,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import qualified Neuron.Backend as Backend
import qualified Neuron.CLI.Logging as Logging
import Neuron.CLI.New (newZettelFile)
import Neuron.CLI.Open (openLocallyGeneratedFile)
import Neuron.CLI.Parser (commandParser)
import Neuron.CLI.Search (interactiveSearch)
import Neuron.CLI.Types
import qualified Neuron.Cache as Cache
import qualified Neuron.Cache.Type as Cache
import qualified Neuron.LSP as LSP
import qualified Neuron.Reactor as Reactor
import qualified Neuron.Version as Version
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Query as Q
import Options.Applicative
import Relude
import System.Directory (getCurrentDirectory)

run :: (Bool -> App ()) -> IO ()
run act = do
  defaultNotesDir <- getCurrentDirectory
  cliParser <- commandParser defaultNotesDir <$> now
  app <-
    execParser $
      info
        (versionOption <*> cliParser <**> helper)
        (fullDesc <> progDesc "Neuron, future-proof Zettelkasten app <https://neuron.zettel.page/>")
  let logAction = Logging.mkLogAction
  runApp (Env app logAction) $ runAppCommand act
  where
    versionOption =
      infoOption
        (toString $ untag Version.neuronVersion)
        (long "version" <> help "Show version")
    now = do
      tz <- getCurrentTimeZone
      utcToLocalTime tz <$> liftIO getCurrentTime

runAppCommand :: (Bool -> App ()) -> App ()
runAppCommand genAct = do
  getCommand >>= \case
    LSP -> do
      LSP.lspServer
    Gen GenCommand {..} -> do
      case serve of
        Just (host, port) -> do
          outDir <- getOutputDir
          appEnv <- getAppEnv
          liftIO $
            race_ (runApp appEnv $ genAct watch) $ do
              runApp appEnv $ Backend.serve host port outDir
        Nothing ->
          genAct watch
    New newCommand ->
      newZettelFile newCommand
    Open openCommand ->
      openLocallyGeneratedFile openCommand
    Query QueryCommand {..} -> do
      Cache.NeuronCache {..} <-
        fmap Cache.stripCache $
          if cached
            then Cache.getCache
            else do
              Reactor.loadZettelkasten >>= \case
                Left e -> fail $ toString e
                Right (ch, _, _) -> pure ch
      case query of
        Left zid -> do
          let result = G.getZettel zid _neuronCache_graph
          putLTextLn $ Aeson.encodeToLazyText result
        Right someQ ->
          withSome someQ $ \q -> do
            let result = Q.runGraphQuery _neuronCache_graph q
            putLTextLn $ Aeson.encodeToLazyText $ Q.graphQueryResultJson q result _neuronCache_errors
    Search searchCmd -> do
      interactiveSearch searchCmd
