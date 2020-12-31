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
import Neuron.CLI.New (newZettelFile)
import Neuron.CLI.Open (openLocallyGeneratedFile)
import Neuron.CLI.Search (interactiveSearch)
import Neuron.CLI.Types
import Neuron.Config (getConfig)
import qualified Neuron.Gen as Gen
import qualified Neuron.Version as Version
import qualified Neuron.Web.Cache as Cache
import qualified Neuron.Web.Cache.Type as Cache
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Query as Q
import Neuron.Zettelkasten.Zettel (sansLinkContext)
import Options.Applicative
import Relude
import System.Directory (getCurrentDirectory)

run :: (Bool -> AppT IO ()) -> IO ()
run act = do
  defaultNotesDir <- getCurrentDirectory
  cliParser <- commandParser defaultNotesDir <$> now
  app <-
    execParser $
      info
        (versionOption <*> cliParser <**> helper)
        (fullDesc <> progDesc "Neuron, future-proof Zettelkasten app <https://neuron.zettel.page/>")
  runAppT app $ runAppCommand act
  where
    versionOption =
      infoOption
        (toString $ untag Version.neuronVersion)
        (long "version" <> help "Show version")
    now = do
      tz <- getCurrentTimeZone
      utcToLocalTime tz <$> liftIO getCurrentTime

runAppCommand :: (Bool -> AppT IO ()) -> AppT IO ()
runAppCommand genAct = do
  c <- cmd <$> getApp
  case c of
    Gen GenCommand {..} -> do
      case serve of
        Just (host, port) -> do
          outDir <- getOutputDir
          app <- getApp
          liftIO $
            race_ (runAppT app $ genAct watch) $ do
              Backend.serve host port outDir
        Nothing ->
          genAct watch
    New newCommand ->
      newZettelFile newCommand =<< getConfig
    Open openCommand ->
      openLocallyGeneratedFile openCommand
    Query QueryCommand {..} -> do
      Cache.NeuronCache {..} <-
        if cached
          then Cache.getCache
          else do
            (ch, _, _) <- Gen.loadZettelkasten =<< getConfig
            pure ch
      case query of
        Left someQ ->
          withSome someQ $ \q -> do
            let zsSmall = sansLinkContext <$> G.getZettels _neuronCache_graph
                result = Q.runZettelQuery zsSmall q
            putLTextLn $ Aeson.encodeToLazyText $ Q.zettelQueryResultJson q result _neuronCache_errors
        Right someQ ->
          withSome someQ $ \q -> do
            let result = Q.runGraphQuery _neuronCache_graph q
            putLTextLn $ Aeson.encodeToLazyText $ Q.graphQueryResultJson q result _neuronCache_errors
    Search searchCmd -> do
      interactiveSearch searchCmd
