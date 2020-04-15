{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import qualified Data.Aeson.Text as Aeson
import qualified Data.Map.Strict as Map
import Development.Shake (Action)
import Neuron.Version (neuronVersion, olderThan)
import qualified Neuron.Version as Version
import Neuron.CLI
import qualified Neuron.Zettelkasten.Config as Z
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

-- | Generate the Zettelkasten site
generateSite ::
  Z.Config ->
  (forall a. Z.Route Z.ZettelStore Z.ZettelGraph a -> (Z.ZettelStore, Z.ZettelGraph, a) -> Action ()) ->
  [FilePath] ->
  Action (Z.ZettelStore, Z.ZettelGraph)
generateSite config writeHtmlRoute' zettelsPat = do
  when (olderThan $ Z.minVersion config) $ do
    error $ "Require neuron mininum version " <> Z.minVersion config <> ", but your neuron version is " <> neuronVersion
  zettelStore <- Z.mkZettelStore =<< Rib.forEvery zettelsPat pure
  let zettelGraph = Z.mkZettelGraph zettelStore
  let writeHtmlRoute v r = writeHtmlRoute' r (zettelStore, zettelGraph, v)
  -- Generate HTML for every zettel
  (writeHtmlRoute () . Z.Route_Zettel) `mapM_` Map.keys zettelStore
  -- Generate the z-index
  writeHtmlRoute () Z.Route_ZIndex
  -- Generate search page
  writeHtmlRoute () Z.Route_Search
  -- Write alias redirects, unless a zettel with that name exists.
  aliases <- Z.getAliases config zettelStore
  forM_ aliases $ \Z.Alias {..} ->
    writeHtmlRoute targetZettel (Z.Route_Redirect aliasZettel)
  pure (zettelStore, zettelGraph)

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
