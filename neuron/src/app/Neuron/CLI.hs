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
import Data.Tagged
import Data.Time
  ( getCurrentTime,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import Neuron.CLI.New (newZettelFile)
import Neuron.CLI.Open (openLocallyGeneratedFile)
import Neuron.CLI.Search (interactiveSearch)
import Neuron.CLI.Types (App (..), AppT, Command (..), QueryCommand (..), commandParser, runAppT)
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

run :: AppT IO () -> IO ()
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
        (toString $ untag Version.neuronVersion)
        (long "version" <> help "Show version")
    now = do
      tz <- getCurrentTimeZone
      utcToLocalTime tz <$> liftIO getCurrentTime

runWith :: AppT IO () -> App -> IO ()
runWith actNg app@App {..} =
  case cmd of
    Gen ->
      runAppT app actNg
    New newCommand ->
      runAppT app $ do
        newZettelFile newCommand =<< getConfig
    Open openCommand ->
      runAppT app $ do
        openLocallyGeneratedFile openCommand
    Query QueryCommand {..} ->
      runAppT app $ do
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
      runAppT app $ do
        interactiveSearch searchCmd
