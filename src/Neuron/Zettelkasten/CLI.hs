{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.CLI
  ( -- * CLI
    App (..),
    Command (..),
    NewCommand (..),
    commandParser,
    runRib,
    runRibOnceQuietly,
  )
where

import Development.Shake (Action, Verbosity (Silent, Verbose))
import qualified Neuron.Zettelkasten.Link.Action as Z
import qualified Neuron.Zettelkasten.Query as Z
import Options.Applicative
import Relude
import qualified Rib.App
import qualified Rib.Cli
import System.Directory
import System.FilePath
import qualified Text.URI as URI

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
  | -- | Open the locally generated Zettelkasten
    Open
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
        ribServe :: Maybe (Text, Int),
        ribQuiet :: Bool,
        ribShakeDbDir :: Maybe FilePath
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
      verbosity = bool Verbose Silent $ ribQuiet cfg
      shakeDbDir = fromMaybe (neuronDir </> ".shake") $ ribShakeDbDir cfg
      watchIgnore = [".neuron", ".git"]
  pure Rib.Cli.CliConfig {..}

runRib :: Action () -> FilePath -> RibConfig -> IO ()
runRib act notesDir ribCfg =
  Rib.App.runWith act =<< mkRibCliConfig notesDir ribCfg

runRibOnceQuietly :: FilePath -> Action () -> IO ()
runRibOnceQuietly notesDir act =
  runRib act notesDir $
    RibConfig
      { ribOutputDir = Nothing, -- Ignoring CLI's output dir. So don't use this within `neuron rib ...`.
        ribWatch = False,
        ribServe = Nothing,
        ribQuiet = True,
        -- Don't want to conflict with a long-running shake build action (eg: rib -wS)
        ribShakeDbDir = Just "/dev/null"
      }

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
            command "open" $ info openCommand $ progDesc "Open the locally generated Zettelkasten website",
            command "search" $ info searchCommand $ progDesc "Search zettels and print the matching filepath",
            command "query" $ info queryCommand $ progDesc "Run a query against the zettelkasten",
            command "rib" $ info ribCommand $ progDesc "Generate static site via rib"
          ]
    newCommand = do
      edit <- switch (long "edit" <> short 'e' <> help "Open the newly-created file in $EDITOR")
      title <- argument str (metavar "TITLE" <> help "Title of the new Zettel")
      return (New NewCommand {..})
    openCommand =
      pure Open
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
      ~(ribQuiet) <- pure False
      ~(ribShakeDbDir) <- pure Nothing
      pure RibConfig {..}
    mkURIMust =
      either (error . toText . displayException) id . URI.mkURI
