{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.CLI
  ( run
  )
where

import qualified Data.Aeson.Text as Aeson
import Development.Shake (Action)
import qualified Neuron.Version as Version
import Neuron.CLI.Types
import Neuron.CLI.Rib
import Neuron.CLI.New (newZettelFile)
import qualified Neuron.Zettelkasten.Query as Z
import qualified Neuron.Zettelkasten.Store as Z
import Options.Applicative
import Relude
import qualified Rib
import System.Directory
import System.FilePath
import System.Info (os)
import System.Posix.Process
import System.Which

neuronSearchScript :: FilePath
neuronSearchScript = $(staticWhich "neuron-search")

searchScriptArgs :: SearchCommand -> [String]
searchScriptArgs SearchCommand {..} =
  let searchByArgs =
        case searchBy of
          SearchByTitle -> ["title: ", "3"]
          SearchByContent -> ["", "2"]
      editArg =
        bool "echo" "$EDITOR" searchEdit
   in searchByArgs <> [editArg]

run :: Action () -> IO ()
run act = do
  defaultNotesDir <- (</> "zettelkasten") <$> getHomeDirectory
  runWith act =<< execParser (opts defaultNotesDir)
  where
    opts d =
      info
        (versionOption <*> commandParser d <**> helper)
        (fullDesc <> progDesc "Neuron, a Zettelkasten CLI <https://neuron.srid.ca/>")
    versionOption =
      infoOption
        (toString Version.neuronVersionFull)
        (long "version" <> help "Show version")

runWith :: Action () -> App -> IO ()
runWith act App {..} = do
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
    Query queries -> do
      runRibOnceQuietly notesDir $ do
        store <- Z.mkZettelStore =<< Rib.forEvery ["*.md"] pure
        let matches = Z.runQuery store queries
        putLTextLn $ Aeson.encodeToLazyText $ matches
    Search searchCmd -> do
      execScript neuronSearchScript $ notesDir : searchScriptArgs searchCmd
  where
    execScript scriptPath args =
      -- We must use the low-level execvp (via the unix package's `executeFile`)
      -- here, such that the new process replaces the current one. fzf won't work
      -- otherwise.
      void $ executeFile scriptPath False args Nothing

