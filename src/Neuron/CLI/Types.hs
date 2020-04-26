{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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

import Data.Some
import Data.TagTree (mkTagPattern)
import qualified Data.Text as T
import Data.Time
import qualified Neuron.Zettelkasten.Query as Z
import Options.Applicative
import Relude
import qualified Rib.Cli
import qualified Text.URI as URI

data App = App
  { notesDir :: FilePath,
    cmd :: Command
  }

data NewCommand = NewCommand
  { title :: Text,
    day :: Day,
    edit :: Bool
  }
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
    Query (Some Z.Query)
  | -- | Delegate to Rib's command parser
    Rib RibConfig

data RibConfig = RibConfig
  { ribOutputDir :: Maybe FilePath,
    ribWatch :: Bool,
    ribServe :: Maybe (Text, Int),
    ribQuiet :: Bool,
    ribShakeDbDir :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | optparse-applicative parser for neuron CLI
commandParser :: FilePath -> Day -> Parser App
commandParser defaultNotesDir today = do
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
      day <-
        option dayReader $
          long "day"
            <> metavar "DAY"
            <> value today
            <> help ("Creation day of the zettel in UTC (default: " <> show today <> ")")
      title <- argument nonEmptyTextReder (metavar "TITLE" <> help "Title of the new Zettel")
      return (New NewCommand {..})
    openCommand =
      pure Open
    queryCommand =
      fmap Query $
        fmap (Some . Z.Query_ZettelsByTag) (many (mkTagPattern <$> option str (long "tag" <> short 't')))
          <|> option queryReader (long "uri" <> short 'u')
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
    queryReader :: ReadM (Some Z.Query)
    queryReader =
      eitherReader $ \(toText -> s) -> case URI.mkURI s of
        Right uri ->
          first show $ Z.queryFromURI uri
        Left e ->
          Left $ displayException e
    nonEmptyTextReder :: ReadM Text
    nonEmptyTextReder =
      eitherReader $ \(T.strip . toText -> s) ->
        if T.null s
          then Left "Empty text is not allowed"
          else Right s
    dayReader :: ReadM Day
    dayReader =
      maybeReader $
        parseTimeM False defaultTimeLocale "%Y-%m-%d"
