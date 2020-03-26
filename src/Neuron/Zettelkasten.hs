{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten
  ( generateSite,
    commandParser,
    run,
    runWith,
    newZettelFile,
  )
where

import qualified Data.Map.Strict as Map
import Development.Shake (Action)
import qualified Neuron.Zettelkasten.Graph as Z
import qualified Neuron.Zettelkasten.ID as Z
import qualified Neuron.Zettelkasten.Route as Z
import qualified Neuron.Zettelkasten.Store as Z
import Options.Applicative
import Path
import Path.IO
import Relude
import qualified Rib
import qualified Rib.App
import System.FilePath (addTrailingPathSeparator, dropTrailingPathSeparator)
import System.Posix.Process
import System.Which

neuronSearchScript :: FilePath
neuronSearchScript = $(staticWhich "neuron-search")

data App
  = App
      { notesDir :: FilePath,
        cmd :: Command
      }
  deriving (Eq, Show)

data Command
  = -- | Create a new zettel file
    New Text
  | -- | Search a zettel by title
    Search
  | Rib Rib.App.Command
  deriving (Eq, Show)

commandParser :: Parser App
commandParser =
  App
    <$> argument (fmap addTrailingPathSeparator str) (metavar "NOTESDIR")
    <*> cmdParser
  where
    cmdParser =
      hsubparser $
        mconcat
          [ command "new" $ info newCommand $ progDesc "Create a new zettel",
            command "search" $ info searchCommand $ progDesc "Search zettels and print the matching filepath",
            command "rib" $ fmap Rib $ info Rib.App.commandParser $ progDesc "Call rib"
          ]
    newCommand =
      New <$> argument str (metavar "TITLE" <> help "Title of the new Zettel")
    searchCommand =
      pure Search

run :: Action () -> IO ()
run act =
  runWith act =<< execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "Zettelkasten based on Rib")

runWith :: Action () -> App -> IO ()
runWith act App {..} = do
  inputDir <- parseRelDir notesDir
  outputDir <- directoryAside inputDir ".output"
  case cmd of
    New tit ->
      putStrLn =<< newZettelFile inputDir tit
    Search ->
      execScript neuronSearchScript [notesDir]
    Rib ribCmd ->
      Rib.App.runWith inputDir outputDir act ribCmd
  where
    execScript scriptPath args =
      -- We must use the low-level execvp (via the unix package's `executeFile`)
      -- here, such that the new process replaces the current one. fzf won't work
      -- otherwise.
      void $ executeFile scriptPath False args Nothing
    directoryAside :: Path Rel Dir -> String -> IO (Path Rel Dir)
    directoryAside fp suffix = do
      let baseName = dropTrailingPathSeparator $ toFilePath $ dirname fp
      newDir <- parseRelDir $ baseName <> suffix
      pure $ parent fp </> newDir

-- | Generate the Zettelkasten site
generateSite ::
  (Z.Route Z.ZettelStore Z.ZettelGraph () -> (Z.ZettelStore, Z.ZettelGraph) -> Action ()) ->
  [Path Rel File] ->
  Action (Z.ZettelStore, Z.ZettelGraph)
generateSite writeHtmlRoute' zettelsPat = do
  zettelStore <- Z.mkZettelStore =<< Rib.forEvery zettelsPat pure
  zettelGraph <- Z.mkZettelGraph zettelStore
  let writeHtmlRoute r = writeHtmlRoute' r (zettelStore, zettelGraph)
  (writeHtmlRoute . Z.Route_Zettel) `mapM_` Map.keys zettelStore
  writeHtmlRoute Z.Route_Index
  pure (zettelStore, zettelGraph)

-- | Create a new zettel file and return its slug
-- TODO: refactor this
newZettelFile :: Path b Dir -> Text -> IO String
newZettelFile inputDir ztitle = do
  zId <- Z.zettelNextIdForToday inputDir
  zettelFileName <- parseRelFile $ toString $ Z.zettelIDSourceFileName zId
  let srcPath = inputDir </> zettelFileName
  doesFileExist srcPath >>= \case
    True ->
      fail $ "File already exists: " <> show srcPath
    False -> do
      writeFile (toFilePath srcPath) $ "---\ntitle: " <> toString ztitle <> "\n---\n\n"
      pure $ toFilePath srcPath
  where
