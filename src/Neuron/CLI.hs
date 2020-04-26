{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI
  ( run,
  )
where

import Data.Time
import Development.Shake (Action)
import Neuron.CLI.New (newZettelFile)
import Neuron.CLI.Query (queryZettelkasten)
import Neuron.CLI.Rib
import Neuron.CLI.Search (interactiveSearch)
import qualified Neuron.Version as Version
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
        (toString Version.neuronVersionFull)
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
    Query q ->
      runRibOnceQuietly notesDir $
        queryZettelkasten notesDir q
    Search searchCmd ->
      interactiveSearch notesDir searchCmd
