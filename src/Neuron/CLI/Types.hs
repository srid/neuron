{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Types
  ( -- * CLI
    App (..),
    Command (..),
    NewCommand (..),
    SearchBy (..),
    SearchCommand (..),
    RibConfig (..),
    commandParser,
  )
where

import qualified Neuron.Zettelkasten.Query as Z
import qualified Neuron.Zettelkasten.Tag as Z
import Options.Applicative
import Relude
import qualified Rib.Cli
import qualified Text.URI as URI

data App = App
  { notesDir :: FilePath,
    cmd :: Command
  }
  deriving (Eq, Show)

data NewCommand = NewCommand {title :: Text, edit :: Bool}
  deriving (Eq, Show)

data SearchCommand = SearchCommand
  { searchBy :: SearchBy,
    searchEdit :: Bool
  }
  deriving (Eq, Show)

data SearchBy
  = SearchByTitle
  | SearchByContent
  deriving (Eq, Show)

data Command
  = -- | Create a new zettel file
    New NewCommand
  | -- | Open the locally generated Zettelkasten
    Open
  | -- | Search a zettel by title
    Search SearchCommand
  | -- | Run a query against the Zettelkasten
    Query [Z.Query]
  | -- | Delegate to Rib's command parser
    Rib RibConfig
  deriving (Eq, Show)

data RibConfig = RibConfig
  { ribOutputDir :: Maybe FilePath,
    ribWatch :: Bool,
    ribServe :: Maybe (Text, Int),
    ribQuiet :: Bool,
    ribShakeDbDir :: Maybe FilePath
  }
  deriving (Eq, Show)

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
      edit <- switch (long "edit" <> short 'e' <> help "Open the newly-created zettel in $EDITOR")
      title <- argument str (metavar "TITLE" <> help "Title of the new Zettel")
      return (New NewCommand {..})
    openCommand =
      pure Open
    queryCommand =
      let tagFilter =
            (Z.ByTag . Z.Tag <$> option str (long "tag" <> short 't'))
              <|> (Z.TagUnder . Z.Tag <$> option str (long "under"))
              <|> (Z.TagFrom . Z.Tag <$> option str (long "from"))
              <|> (Z.TagGlob . Z.TagPattern <$> option str (long "glob"))
       in fmap Query $
            many tagFilter <|> option uriReader (long "uri" <> short 'u')
    searchCommand = do
      searchBy <-
        bool SearchByTitle SearchByContent
          <$> switch (long "full-text" <> short 'a' <> help "Full-text search")
      edit <- switch (long "edit" <> short 'e' <> help "Open the matching zettel in $EDITOR")
      pure $ Search $ SearchCommand searchBy edit
    ribCommand = fmap Rib $ do
      let ribQuiet = False
          ribShakeDbDir = Nothing
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
    uriReader =
      eitherReader $ bimap displayException Z.parseQuery . URI.mkURI . toText
