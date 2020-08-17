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
    OpenCommand (..),
    QueryCommand (..),
    RibConfig (..),
    commandParser,
  )
where

import Data.Default (def)
import Data.Some
import Data.TagTree (mkTagPattern)
import Data.Time
import Neuron.Reader.Type (ZettelFormat)
import qualified Neuron.Web.Route as R
import qualified Neuron.Zettelkasten.Connection as C
import Neuron.Zettelkasten.ID (ZettelID, parseZettelID')
import Neuron.Zettelkasten.ID.Scheme (IDScheme (..))
import Neuron.Zettelkasten.Query.Graph as Q
import qualified Neuron.Zettelkasten.Query.Parser as Q
import Neuron.Zettelkasten.Zettel as Q
import Neuron.Zettelkasten.Zettel.Meta (parseZettelDate)
import Options.Applicative
import Relude
import qualified Rib.Cli
import qualified Text.URI as URI

data App = App
  { notesDir :: FilePath,
    cmd :: Command
  }

data NewCommand = NewCommand
  { title :: Maybe Text,
    format :: Maybe ZettelFormat,
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

data OpenCommand = OpenCommand
  { route :: Some R.Route
  }
  deriving (Eq, Show)

data QueryCommand = QueryCommand
  { -- Use cache instead of building the zettelkasten from scratch
    cached :: Bool,
    query :: Either (Some Q.ZettelQuery) (Some Q.GraphQuery)
  }
  deriving (Eq, Show)

data Command
  = -- | Create a new zettel file
    New NewCommand
  | -- | Open the locally generated Zettelkasten
    Open OpenCommand
  | -- | Search a zettel by title
    Search SearchCommand
  | -- | Run a query against the Zettelkasten
    Query QueryCommand
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
      title <- optional $ strArgument (metavar "TITLE" <> help "Title of the new Zettel")
      format <-
        optional $
          option auto $
            metavar "FORMAT"
              <> short 'f'
              <> long "format"
              <> help "The document format of the new zettel"
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
      pure $ New $ NewCommand title format day (idSchemeF day) edit
    openCommand = do
      fmap Open $
        fmap
          (const $ OpenCommand $ Some $ R.Route_ZIndex)
          (switch (long "zindex" <> help "Open z-index"))
          <|> fmap
            (const $ OpenCommand $ Some $ R.Route_Search Nothing)
            (switch (long "search" <> help "Open the search page"))
          <|> fmap
            (OpenCommand . Some . R.Route_Zettel)
            (option zettelIDReader (long "id" <> help "Open the zettel HTML page" <> metavar "ID"))
    queryCommand = do
      cached <- switch (long "cached" <> help "Use cached zettelkasten graph (faster)")
      query <-
        fmap
          Left
          ( fmap
              (Some . flip Q.ZettelQuery_ZettelByID Nothing)
              (option zettelIDReader (long "id"))
              <|> fmap
                (\x -> Some $ Q.ZettelQuery_ZettelsByTag x Nothing def)
                (many (mkTagPattern <$> option str (long "tag" <> short 't')))
              <|> option queryReader (long "uri" <> short 'u')
          )
          <|> fmap
            Right
            ( fmap
                (const $ Some $ Q.GraphQuery_Id)
                ( switch $
                    long "graph" <> help "Get the entire zettelkasten graph as JSON"
                )
                <|> fmap
                  (Some . Q.GraphQuery_BacklinksOf Nothing)
                  ( option
                      zettelIDReader
                      ( long "backlinks-of"
                          <> help "Get backlinks to the given zettel ID"
                          <> metavar "ID"
                      )
                  )
                <|> fmap
                  (Some . Q.GraphQuery_BacklinksOf (Just C.Folgezettel))
                  ( option
                      zettelIDReader
                      ( long "uplinks-of"
                          <> help "Get uplinks to the given zettel ID"
                          <> metavar "ID"
                      )
                  )
            )
      pure $ Query $ QueryCommand {..}
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
    queryReader :: ReadM (Some Q.ZettelQuery)
    queryReader =
      eitherReader $ \(toText -> s) -> case URI.mkURI s of
        Right uri ->
          maybe (Left "Unsupported query") Right $ Q.queryFromURI uri
        Left e ->
          Left $ displayException e
    dayReader :: ReadM Day
    dayReader =
      maybeReader (parseZettelDate . toText)
