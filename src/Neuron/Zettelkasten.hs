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
import Development.Shake (Verbosity (Silent, Verbose))
import qualified Neuron.Version as Version
import qualified Neuron.Zettelkasten.Graph as Z
import qualified Neuron.Zettelkasten.ID as Z
import qualified Neuron.Zettelkasten.Link.Action as Z
import qualified Neuron.Zettelkasten.Query as Z
import qualified Neuron.Zettelkasten.Route as Z
import qualified Neuron.Zettelkasten.Store as Z
import Options.Applicative
import Relude
import qualified Rib
import qualified Rib.App
import qualified Rib.Cli
import System.Directory
import System.FilePath
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
    Rib RibConfig
  deriving (Eq, Show)

data RibConfig
  = RibConfig
      { ribOutputDir :: Maybe FilePath,
        ribWatch :: Bool,
        ribServe :: Maybe (Text, Int)
      }
  deriving (Eq, Show)

mkRibCliConfig :: FilePath -> RibConfig -> IO Rib.Cli.CliConfig
mkRibCliConfig inputDir cfg = do
  unlessM (doesDirectoryExist inputDir) $ do
    fail $ "Zettelkasten directory " <> inputDir <> " does not exist."
  let neuronDir = inputDir </> ".neuron"
      outputDir = fromMaybe (neuronDir </> "output") $ ribOutputDir cfg
      rebuildAll = True
      watch = ribWatch cfg
      serve = ribServe cfg
      verbosity = Verbose
      shakeDbDir = neuronDir </> ".shake"
      watchIgnore = [".neuron", ".git"]
  pure Rib.Cli.CliConfig {..}

-- | optparse-applicative parser for neuron CLI
commandParser :: FilePath -> Parser App
commandParser defaultNotesDir = do
  notesDir <-
    option
      Rib.Cli.directoryReader
      ( long "zettelkasten-dir" <> short 'd' <> metavar "NOTESDIR" <> value defaultNotesDir
          <> help ("Your zettelkasten directory containing the zettel files (" <> "default: " <> defaultNotesDir <> ")")
      )
  cmd <- cmdParser
  pure $ App {..}
  where
    cmdParser =
      hsubparser $
        mconcat
          [ command "new" $ info newCommand $ progDesc "Create a new zettel",
            command "search" $ info searchCommand $ progDesc "Search zettels and print the matching filepath",
            command "query" $ info queryCommand $ progDesc "Run a query against the zettelkasten",
            command "rib" $ info ribCommand $ progDesc "Generate static site via rib"
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
    ribCommand = fmap Rib $ do
      ribOutputDir <-
        optional $
          option
            Rib.Cli.directoryReader
            ( long "output-dir" <> short 'o' <> metavar "OUTPUTDIR"
                <> help ("The directory where HTML will be generated (" <> "default: NOTESDIR/.neuron/output)")
            )
      ribWatch <- Rib.Cli.watchOption
      ribServe <- Rib.Cli.serveOption
      pure RibConfig {..}
    mkURIMust =
      either (error . toText . displayException) id . URI.mkURI

run :: Action () -> IO ()
run act = do
  defaultNotesDir <- (</> "zettelkasten") <$> getHomeDirectory
  runWith act =<< execParser (opts defaultNotesDir)
  where
    opts d =
      info
        (versionOption <*> commandParser d <**> helper)
        (fullDesc <> progDesc "Zettelkasten based on Rib")
    versionOption =
      infoOption
        (toString Version.neuronVersionFull)
        (long "version" <> help "Show version")

runWith :: Action () -> App -> IO ()
runWith act App {..} = do
  case cmd of
    New newCommand ->
      newZettelFile notesDir newCommand
    Search ->
      execScript neuronSearchScript [notesDir]
    Query queries -> do
      cfg <- oneOffCfg
      flip Rib.App.runWith cfg $ do
        store <- Z.mkZettelStore =<< Rib.forEvery ["*.md"] pure
        let matches = Z.runQuery store queries
        putLTextLn $ Aeson.encodeToLazyText $ matches
    Rib ribCfg ->
      Rib.App.runWith act =<< mkRibCliConfig notesDir ribCfg
  where
    oneOffCfg = do
      cfg <- mkRibCliConfig notesDir $ RibConfig Nothing False Nothing
      pure $
        cfg
          { Rib.Cli.verbosity = Silent
          }
    execScript scriptPath args =
      -- We must use the low-level execvp (via the unix package's `executeFile`)
      -- here, such that the new process replaces the current one. fzf won't work
      -- otherwise.
      void $ executeFile scriptPath False args Nothing

-- | Generate the Zettelkasten site
generateSite ::
  (Z.Route Z.ZettelStore Z.ZettelGraph () -> (Z.ZettelStore, Z.ZettelGraph) -> Action ()) ->
  [FilePath] ->
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
newZettelFile :: FilePath -> NewCommand -> IO ()
newZettelFile inputDir NewCommand {..} = do
  -- TODO: refactor this function
  zId <- Z.zettelNextIdForToday inputDir
  let zettelFileName = toString $ Z.zettelIDSourceFileName zId
  let srcPath = inputDir </> zettelFileName
  doesFileExist srcPath >>= \case
    True ->
      fail $ "File already exists: " <> show srcPath
    False -> do
      writeFile srcPath $ "---\ntitle: " <> toString title <> "\n---\n\n"
      putStrLn srcPath
      when edit $ do
        getEnvNonEmpty "EDITOR" >>= \case
          Nothing -> do
            die "\nCan't open file; you must set the EDITOR environment variable"
          Just editor -> do
            executeFile editor True [srcPath] Nothing
  where
    getEnvNonEmpty name =
      Env.getEnv name >>= \case
        Nothing -> pure Nothing
        Just "" -> pure Nothing
        Just v -> pure $ Just v
