{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Zettelkasten
  ( generateSite,
    run,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Map.Strict as Map
import Development.Shake (Action)
import qualified Neuron.Version as Version
import Neuron.Zettelkasten.CLI (App (..), Command (..), NewCommand (..), commandParser, runRib, runRibOnceQuietly)
import qualified Neuron.Zettelkasten.Graph as Z
import qualified Neuron.Zettelkasten.ID as Z
import qualified Neuron.Zettelkasten.Query as Z
import qualified Neuron.Zettelkasten.Route as Z
import qualified Neuron.Zettelkasten.Store as Z
import Options.Applicative
import Relude
import qualified Rib
import System.Directory
import System.FilePath
import System.Info (os)
import qualified System.Posix.Env as Env
import System.Posix.Process
import System.Which

neuronSearchScript :: FilePath
neuronSearchScript = $(staticWhich "neuron-search")

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
    Search ->
      execScript neuronSearchScript [notesDir]
  where
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
  -- Generate zettelkasten index and search page
  writeIndex zettelStore
  writeHtmlRoute (Z.Route_Search Nothing [])
  -- Write index.html, unless a index.md zettel exists
  when (isNothing $ Map.lookup (Z.parseZettelID "index") zettelStore) $
    writeHtmlRoute Z.Route_IndexRedirect
  pure (zettelStore, zettelGraph)

writeIndex :: Z.ZettelStore -> Action ()
writeIndex store = do
  let results = Z.runQuery store []
  Rib.writeFileCached "index.json" $ decodeUtf8 $ Aeson.encode (Aeson.toJSON results)

-- | Create a new zettel file and open it in editor if requested
--
-- As well as print the path to the created file.
newZettelFile :: NewCommand -> Action ()
newZettelFile NewCommand {..} = do
  zId <- Z.zettelNextIdForToday
  let zettelFileName = toString $ Z.zettelIDSourceFileName zId
  inputDir <- Rib.ribInputDir
  let srcPath = inputDir </> zettelFileName
  liftIO $
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
