{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Zettelkasten
  ( -- * CLI
    App (..),
    NewCommand (..),
    commandParser,
    run,
    runWith,

    -- * Rib site generation
    generateSite,

    -- * Etc
    newZettelFile,
  )
where

import qualified Data.Aeson.Text as Aeson
import qualified Data.Map.Strict as Map
import Development.Shake (Action)
import qualified Neuron.Version as Version
import qualified Neuron.Zettelkasten.Graph as Z
import qualified Neuron.Zettelkasten.ID as Z
import qualified Neuron.Zettelkasten.Link.Action as Z
import qualified Neuron.Zettelkasten.Query as Z
import qualified Neuron.Zettelkasten.Route as Z
import qualified Neuron.Zettelkasten.Store as Z
import Options.Applicative
import Path
import Path.IO
import Relude
import qualified Rib
import qualified Rib.App
import qualified System.Directory as Directory
import System.FilePath (addTrailingPathSeparator, dropTrailingPathSeparator)
import qualified System.Posix.Env as Env
import System.Posix.Process
import System.Which
import qualified Text.URI as URI

neuronSearchScript :: FilePath
neuronSearchScript = $(staticWhich "neuron-search")

data App
  = App
      { notesDir :: FilePath,
        cmd :: Command
      }
  deriving (Eq, Show)

data NewCommand = NewCommand {title :: Text, edit :: Bool}
  deriving (Eq, Show)

data Command
  = -- | Create a new zettel file
    New NewCommand
  | -- | Search a zettel by title
    Search
  | -- | Run a query against the Zettelkasten
    Query [Z.Query]
  | -- | Delegate to Rib's command parser
    Rib Rib.App.Command
  deriving (Eq, Show)

-- | optparse-applicative parser for neuron CLI
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
            command "query" $ info queryCommand $ progDesc "Run a query against the zettelkasten",
            command "rib" $ fmap Rib $ info Rib.App.commandParser $ progDesc "Run a rib command"
          ]
    newCommand = do
      edit <- switch (long "edit" <> short 'e' <> help "Open the newly-created file in $EDITOR")
      title <- argument str (metavar "TITLE" <> help "Title of the new Zettel")
      return (New NewCommand {..})
    queryCommand =
      fmap Query $
        (many (Z.ByTag <$> option str (long "tag" <> short 't')))
          <|> (Z.queryFromUri . mkURIMust <$> option str (long "uri" <> short 'u'))
    searchCommand =
      pure Search
    mkURIMust =
      either (error . toText . displayException) id . URI.mkURI

run :: Action () -> IO ()
run act =
  runWith act =<< execParser opts
  where
    opts =
      info
        (versionOption <*> commandParser <**> helper)
        (fullDesc <> progDesc "Zettelkasten based on Rib")
    versionOption =
      infoOption
        (toString Version.neuronVersionFull)
        (long "version" <> help "Show version")

runWith :: Action () -> App -> IO ()
runWith ribAction App {..} = do
  notesDirAbs <- Directory.makeAbsolute notesDir
  inputDir <- parseAbsDir notesDirAbs
  outputDir <- directoryAside inputDir ".output"
  case cmd of
    New newCommand ->
      newZettelFile inputDir newCommand
    Search ->
      execScript neuronSearchScript [notesDir]
    Query queries -> do
      runRibOneOffShake inputDir outputDir $ do
        store <- Z.mkZettelStore =<< Rib.forEvery [[relfile|*.md|]] pure
        let matches = Z.runQuery store queries
        putLTextLn $ Aeson.encodeToLazyText $ matches
    Rib ribCmd ->
      runRib inputDir outputDir ribCmd ribAction
  where
    execScript scriptPath args =
      -- We must use the low-level execvp (via the unix package's `executeFile`)
      -- here, such that the new process replaces the current one. fzf won't work
      -- otherwise.
      void $ executeFile scriptPath False args Nothing
    -- Run an one-off shake action through rib
    runRibOneOffShake inputDir outputDir =
      runRib inputDir outputDir Rib.App.OneOff
    runRib inputDir outputDir ribCmd act =
      -- CD to the parent of notes directory, because Rib API takes only
      -- relative path
      withCurrentDir (parent inputDir) $ do
        inputDirRel <- makeRelativeToCurrentDir inputDir
        outputDirRel <- makeRelativeToCurrentDir outputDir
        Rib.App.runWith inputDirRel outputDirRel act ribCmd
    directoryAside :: Path Abs Dir -> String -> IO (Path Abs Dir)
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
  let zettelGraph = Z.mkZettelGraph zettelStore
  let writeHtmlRoute r = writeHtmlRoute' r (zettelStore, zettelGraph)
  -- Generate HTML for every zettel
  (writeHtmlRoute . Z.Route_Zettel) `mapM_` Map.keys zettelStore
  -- Generate the z-index
  writeHtmlRoute Z.Route_ZIndex
  -- Write index.html, unless a index.md zettel exists
  when (isNothing $ Map.lookup (Z.parseZettelID "index") zettelStore) $
    writeHtmlRoute Z.Route_IndexRedirect
  pure (zettelStore, zettelGraph)

-- | Create a new zettel file and open it in editor if requested
--
-- As well as print the path to the created file.
newZettelFile :: Path b Dir -> NewCommand -> IO ()
newZettelFile inputDir NewCommand {..} = do
  -- TODO: refactor this function
  zId <- Z.zettelNextIdForToday inputDir
  zettelFileName <- parseRelFile $ toString $ Z.zettelIDSourceFileName zId
  let srcPath = inputDir </> zettelFileName
  doesFileExist srcPath >>= \case
    True ->
      fail $ "File already exists: " <> show srcPath
    False -> do
      writeFile (toFilePath srcPath) $ "---\ntitle: " <> toString title <> "\n---\n\n"
      let path = toFilePath srcPath
      putStrLn path
      when edit $ do
        getEnvNonEmpty "EDITOR" >>= \case
          Nothing -> do
            die "\nCan't open file; you must set the EDITOR environment variable"
          Just editor -> do
            executeFile editor True [path] Nothing
  where
    getEnvNonEmpty name =
      Env.getEnv name >>= \case
        Nothing -> pure Nothing
        Just "" -> pure Nothing
        Just v -> pure $ Just v
