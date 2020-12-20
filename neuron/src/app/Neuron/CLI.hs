{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI
  ( run,
  )
where

import qualified Data.Aeson.Text as Aeson
import Data.Some (withSome)
import Data.Time
  ( getCurrentTime,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import Development.Shake (Action)
import Neuron.CLI.New (newZettelFile)
import Neuron.CLI.Open (openLocallyGeneratedFile)
import Neuron.CLI.Rib
import Neuron.CLI.Search (interactiveSearch)
import Neuron.CLI.Types (QueryCommand (..))
import Neuron.Config (getConfig)
import Neuron.Config.Type (Config)
import qualified Neuron.Version as Version
import qualified Neuron.Web.Cache as Cache
import qualified Neuron.Web.Cache.Type as Cache
import qualified Neuron.Web.Generate as Gen
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Query as Q
import Options.Applicative
import Relude
import System.Directory (getCurrentDirectory)

run :: (Config -> Action ()) -> IO ()
run act = do
  defaultNotesDir <- getCurrentDirectory
  cliParser <- commandParser defaultNotesDir <$> now
  app <-
    execParser $
      info
        (versionOption <*> cliParser <**> helper)
        (fullDesc <> progDesc "Neuron, future-proof Zettelkasten app <https://neuron.zettel.page/>")
  runWith act app
  where
    versionOption =
      infoOption
        (toString Version.neuronVersion)
        (long "version" <> help "Show version")
    now = do
      tz <- getCurrentTimeZone
      utcToLocalTime tz <$> liftIO getCurrentTime

runWith :: (Config -> Action ()) -> App -> IO ()
runWith act App {..} =
  case cmd of
    Rib ribCfg ->
      runRib (act =<< getConfig) notesDir ribCfg
    New newCommand ->
      runRibOnceQuietly notesDir $ do
        newZettelFile newCommand =<< getConfig
    Open openCommand ->
      runRibOnceQuietly notesDir $ do
        openLocallyGeneratedFile openCommand
    Query QueryCommand {..} ->
      runRibOnceQuietly notesDir $ do
        Cache.NeuronCache {..} <-
          if cached
            then Cache.getCache
            else Gen.loadZettelkastenGraph =<< getConfig
        case query of
          Left someQ ->
            withSome someQ $ \q -> do
              let result = Q.runZettelQuery (G.getZettels _neuronCache_graph) q
              putLTextLn $ Aeson.encodeToLazyText $ Q.zettelQueryResultJson q result _neuronCache_errors
          Right someQ ->
            withSome someQ $ \q -> do
              let result = Q.runGraphQuery _neuronCache_graph q
              putLTextLn $ Aeson.encodeToLazyText $ Q.graphQueryResultJson q result _neuronCache_errors
    Search searchCmd -> do
      runRibOnceQuietly notesDir $ do
        interactiveSearch notesDir searchCmd =<< getConfig
