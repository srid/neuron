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

import Data.Default (def)
import Data.Some
import Data.TagTree (mkTagPattern)
import qualified Data.Text as T
import Data.Time
import Neuron.Zettelkasten.ID (ZettelID, parseZettelID')
import Neuron.Zettelkasten.ID.Scheme (IDScheme (..))
import qualified Neuron.Zettelkasten.Query.Error as Q
import qualified Neuron.Zettelkasten.Query.Parser as Q
import Neuron.Zettelkasten.Query.Type as Q
import Neuron.Zettelkasten.Zettel.Meta (zettelDateFormat)
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
    idScheme :: Some IDScheme,
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
    Query (Some Q.Query)
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
      ( long "zettelkasten-dir" <> short 'd' <> metavar "NOTESDIR" <> value defaultNotesDir <> showDefault
          <> help "Your zettelkasten directory containing the zettel files"
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
      title <- argument nonEmptyTextReader (metavar "TITLE" <> help "Title of the new Zettel")
      edit <- switch (long "edit" <> short 'e' <> help "Open the newly-created zettel in $EDITOR")
      day <-
        option dayReader $
          long "day"
            <> metavar "DAY"
            <> value today
            <> showDefault
            <> help "Zettel creation date in UTC"
      -- NOTE: optparse-applicative picks the first option as the default.
      idSchemeF <-
        fmap
          (const $ const $ Some IDSchemeHash)
          (switch (long "id-hash" <> help "Use random hash ID (default)"))
          <|> fmap
            (const $ Some . IDSchemeDate)
            (switch (long "id-date" <> help "Use date encoded ID"))
          <|> fmap
            (const . Some . IDSchemeCustom)
            (option str (long "id" <> help "Use a custom ID" <> metavar "IDNAME"))
      pure $ New $ NewCommand title day (idSchemeF day) edit
    openCommand =
      pure Open
    queryCommand =
      fmap Query $
        fmap (Some . flip Q.Query_ZettelByID Nothing) (option zettelIDReader (long "id"))
          <|> fmap (\x -> Some $ Q.Query_ZettelsByTag x Nothing def) (many (mkTagPattern <$> option str (long "tag" <> short 't')))
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
            ( long "output-dir" <> short 'o' <> metavar "OUTPUTDIR" <> showDefault
                <> help "The directory where HTML will be generated"
            )
      ribWatch <- Rib.Cli.watchOption
      ribServe <- Rib.Cli.serveOption
      pure RibConfig {..}
    zettelIDReader :: ReadM ZettelID
    zettelIDReader =
      eitherReader $ first show . parseZettelID' . toText
    queryReader :: ReadM (Some Q.Query)
    queryReader =
      eitherReader $ \(toText -> s) -> case URI.mkURI s of
        Right uri ->
          either (Left . toString . Q.showQueryParseError) (maybe (Left "Unsupported query") Right) $ Q.queryFromURI uri
        Left e ->
          Left $ displayException e
    nonEmptyTextReader :: ReadM Text
    nonEmptyTextReader =
      eitherReader $ \(T.strip . toText -> s) ->
        if T.null s
          then Left "Empty text is not allowed"
          else Right s
    dayReader :: ReadM Day
    dayReader =
      maybeReader $
        parseTimeM False defaultTimeLocale zettelDateFormat
