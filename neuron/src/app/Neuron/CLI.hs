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
import Data.Some
import Data.Time
import Development.Shake (Action)
import Neuron.CLI.New (newZettelFile)
import Neuron.CLI.Rib
import Neuron.CLI.Search (interactiveSearch)
import qualified Neuron.Version as Version
import qualified Neuron.Web.Generate as Gen
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Query as Q
import Options.Applicative
import Relude
import qualified Rib
import System.Directory
import System.FilePath
import System.Info (os)
import System.Posix.Process

run :: Action () -> IO ()
run act = do
  today <- utctDay <$> liftIO getCurrentTime
  defaultNotesDir <- (</> "zettelkasten") <$> getHomeDirectory
  let cliParser = commandParser defaultNotesDir today
  app <-
    execParser $
      info
        (versionOption <*> cliParser <**> helper)
        (fullDesc <> progDesc "Neuron, a Zettelkasten CLI <https://neuron.zettel.page/>")
  runWith act app
  where
    versionOption =
      infoOption
        (toString Version.neuronVersion)
        (long "version" <> help "Show version")

runWith :: Action () -> App -> IO ()
runWith act App {..} =
  case cmd of
    Rib ribCfg ->
      runRib act notesDir ribCfg
    New newCommand ->
      runRibOnceQuietly notesDir $ do
        newZettelFile newCommand
    Open ->
      runRibOnceQuietly notesDir $ do
        indexHtmlPath <- fmap (</> "index.html") Rib.ribOutputDir
        putStrLn indexHtmlPath
        let opener = if os == "darwin" then "open" else "xdg-open"
        liftIO $ executeFile opener True [indexHtmlPath] Nothing
    Query eSomeQ ->
      runRibOnceQuietly notesDir $ do
        (graph, _, errors) <- Gen.loadZettelkasten
        case eSomeQ of
          Left someQ ->
            withSome someQ $ \q -> do
              let result = Q.runZettelQuery (G.getZettels graph) q
              putLTextLn $ Aeson.encodeToLazyText $ Q.zettelQueryResultJson q result errors
          Right someQ ->
            withSome someQ $ \q -> do
              let result = Q.runGraphQuery graph q
              putLTextLn $ Aeson.encodeToLazyText $ Q.graphQueryResultJson q result errors
    Search searchCmd ->
      interactiveSearch notesDir searchCmd
