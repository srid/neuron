{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Rib
  ( -- * CLI
    App (..),
    Command (..),
    NewCommand (..),
    SearchBy (..),
    SearchCommand (..),
    commandParser,
    runRib,
    runRibOnceQuietly,
  )
where

import Development.Shake (Action, Verbosity (Silent, Verbose))
import Neuron.CLI.Types
import Relude
import qualified Rib.App
import qualified Rib.Cli
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

runRib :: Action () -> FilePath -> RibConfig -> IO ()
runRib act notesDir ribCfg =
  Rib.App.runWith act =<< mkRibCliConfig notesDir ribCfg
  where
    mkRibCliConfig :: FilePath -> RibConfig -> IO Rib.Cli.CliConfig
    mkRibCliConfig inputDir cfg = do
      unlessM (doesDirectoryExist inputDir) $ do
        fail $ "Zettelkasten directory (" <> inputDir <> ") does not exist. You may create it manually using `mkdir " <> inputDir <> "` before running neuron."
      let neuronDir = inputDir </> ".neuron"
          outputDir = fromMaybe (neuronDir </> "output") $ ribOutputDir cfg
          rebuildAll = False
          watch = ribWatch cfg
          serve = ribServe cfg
          verbosity = bool Verbose Silent $ ribQuiet cfg
          shakeDbDir = fromMaybe (neuronDir </> ".shake") $ ribShakeDbDir cfg
          watchIgnore = [".neuron", ".git"]
      pure Rib.Cli.CliConfig {..}

runRibOnceQuietly :: FilePath -> Action () -> IO ()
runRibOnceQuietly notesDir act =
  runRib act notesDir ribOneOffConfig
  where
    ribOneOffConfig :: RibConfig
    ribOneOffConfig =
      RibConfig
        { ribOutputDir = Nothing, -- Ignoring CLI's output dir. So don't use this within `neuron rib ...`.
          ribWatch = False,
          ribServe = Nothing,
          ribQuiet = True,
          -- Don't want to conflict with a long-running shake build action (eg: rib -wS)
          ribShakeDbDir = Just "/dev/null"
        }
