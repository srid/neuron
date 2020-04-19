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

import Data.Aeson
import qualified Data.Aeson.Text as Aeson
import Data.Some
import Development.Shake (Action)
import Neuron.CLI.New (newZettelFile)
import Neuron.CLI.Rib
import Neuron.CLI.Search (runSearch)
import qualified Neuron.Version as Version
import Neuron.Zettelkasten.ID (zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.Query as Z
import qualified Neuron.Zettelkasten.Store as Z
import Neuron.Zettelkasten.Zettel (Zettel (..), zettelJson)
import Options.Applicative
import Relude
import qualified Rib
import System.Directory
import System.FilePath
import System.Info (os)
import System.Posix.Process

run :: Action () -> IO ()
run act = do
  defaultNotesDir <- (</> "zettelkasten") <$> getHomeDirectory
  runWith act =<< execParser (opts defaultNotesDir)
  where
    opts d =
      info
        (versionOption <*> commandParser d <**> helper)
        (fullDesc <> progDesc "Neuron, a Zettelkasten CLI <https://neuron.zettel.page/>")
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
    Query q ->
      runRibOnceQuietly notesDir $ do
        store <- Z.mkZettelStore =<< Rib.forEvery ["*.md"] pure
        case q of
          Some (Z.Query_ZettelByID zid) -> do
            let res = Z.lookupStore zid store
            putLTextLn $ Aeson.encodeToLazyText $ zettelJsonWith res
          Some (Z.Query_ZettelsByTag pats) -> do
            let res = Z.runQuery store (Z.Query_ZettelsByTag pats)
            putLTextLn $ Aeson.encodeToLazyText $ zettelJsonWith <$> res
          Some (Z.Query_Tags pats) -> do
            let res = Z.runQuery store (Z.Query_Tags pats)
            putLTextLn $ Aeson.encodeToLazyText res
    Search searchCmd ->
      runSearch notesDir searchCmd
  where
    zettelJsonWith z@Zettel {..} =
      object $
        [ "path" .= (notesDir </> zettelIDSourceFileName zettelID)
        ]
          <> zettelJson z
