{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Parser
  ( commandParser,
  )
where

import Data.Some (Some (..))
import qualified Data.TagTree as TagTree
import Data.Time (LocalTime)
import Data.Time.DateMayTime
  ( DateMayTime,
    formatDateMayTime,
    mkDateMayTime,
    parseDateMayTime,
  )
import Neuron.CLI.Types
import qualified Neuron.Frontend.Route as R
import qualified Neuron.Zettelkasten.Connection as C
import Neuron.Zettelkasten.ID (ZettelID, parseZettelID)
import Neuron.Zettelkasten.ID.Scheme (IDScheme (..))
import Neuron.Zettelkasten.Query.Graph as Q (GraphQuery (..))
import Options.Applicative
import Options.Applicative.Extra
  ( directoryReader,
    hostPortOption,
  )
import Relude

-- | optparse-applicative parser for neuron CLI
commandParser :: FilePath -> LocalTime -> Parser AppConfig
commandParser defaultNotesDir now = do
  notesDir <-
    option
      directoryReader
      ( short 'd' <> metavar "PATH" <> value defaultNotesDir
          <> help "Run as if neuron was started in PATH instead of the current working directory"
      )
  outputDir <-
    optional $
      option
        directoryReader
        ( short 'o' <> metavar "PATH"
            <> help "Custom path to generate static site in"
        )
  cmd <- cmdParser <|> cmdParserHidden
  pure $ AppConfig {..}
  where
    cmdParser =
      hsubparser $
        mconcat
          [ command "gen" (info genCommand $ progDesc "Generate and serve the static site"),
            command "new" $ info newCommand $ progDesc "Create a new zettel",
            command "open" $ info openCommand $ progDesc "Open the local static site",
            command "search" $ info searchCommand $ progDesc "Search zettels and print their path",
            command "query" $ info queryCommand $ progDesc "Query the zettelkasten in JSON"
          ]
    -- Old commands are retained (but as alias) for backwards compat
    cmdParserHidden =
      hsubparser $
        mconcat
          [ command "rib" (info genCommand $ progDesc "Alias to gen"),
            command "lsp" $ info lspCommand $ progDesc "Run LSP server (WIP)", -- LSP is WIP
            internal
          ]
    lspCommand =
      pure LSP
    newCommand = do
      idScheme <-
        fmap (maybe (Some IDSchemeHash) (Some . IDSchemeCustom)) $
          optional $
            strArgument (metavar "TITLEID" <> help "Custom (title) ID to use; otherwise random ID will be generated")
      edit <- switch (long "edit" <> short 'e' <> help "Open the newly-created zettel in $EDITOR")
      dateParam <-
        option dateReader $
          long "date"
            <> metavar "DATE/TIME"
            <> value (mkDateMayTime $ Right now)
            <> showDefaultWith (toString . formatDateMayTime)
            <> help "Zettel date/time"
      pure $ New $ NewCommand dateParam idScheme edit
    openCommand = do
      fmap Open $
        fmap
          (const $ OpenCommand $ Some R.Route_Impulse)
          (switch (long "search" <> help "Open the search page"))
          <|> fmap
            (OpenCommand . Some . R.Route_Zettel)
            (strOption (long "slug" <> help "Open the zettel HTML page" <> metavar "SLUG"))
    queryCommand = do
      cached <- switch (long "cached" <> help "Use cached zettelkasten graph (faster)")
      jsonl <- switch (long "jsonl" <> help "output JSONL format (better for tool processing)")
      query <-
        fmap CliQuery_ById (option zettelIDReader (long "id" <> metavar "ID" <> help "Get a zettel by its ID"))
          <|> fmap (const CliQuery_Zettels) (switch $ long "zettels" <> help "Get all zettels")
          <|> fmap (const CliQuery_Tags) (switch $ long "tags" <> help "Get all tags (fails if tags plugin is not enabled)")
          <|> fmap (CliQuery_ByTag . TagTree.Tag) (strOption $ long "tag" <> metavar "TAG" <> help "Get zettels by tag (fails if tags plugin is not enabled)")
          <|> fmap
            CliQuery_Graph
            ( fmap
                (const $ Some Q.GraphQuery_Id)
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
    genCommand :: Parser Command
    genCommand = do
      serveCmd <- fmap (uncurry ServeCommand) <$> hostPortOption
      watch <-
        switch
          ( long "watch"
              <> short 'w'
              <> help "Watch for changes and regenerate"
          )
      usePrettyUrls <-
        switch (long "pretty-urls" <> help "Drop the .html at the end of Zettel URLs")
      pure $ Gen (serveCmd, GenCommand {..})
    zettelIDReader :: ReadM ZettelID
    zettelIDReader =
      eitherReader $ first show . parseZettelID . toText
    dateReader :: ReadM DateMayTime
    dateReader =
      maybeReader (parseDateMayTime . toText)
