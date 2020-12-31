{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Parser
  ( commandParser,
  )
where

import Data.Default (def)
import Data.Some (Some (..))
import Data.TagTree (mkDefaultTagQuery, mkTagPattern)
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
import qualified Neuron.Zettelkasten.Query.Parser as Q
import Neuron.Zettelkasten.Zettel as Q
  ( ZettelQuery (..),
  )
import Options.Applicative
import Options.Applicative.Extra
import Relude
import qualified Text.URI as URI

-- | optparse-applicative parser for neuron CLI
commandParser :: FilePath -> LocalTime -> Parser App
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
  cmd <- cmdParser
  pure $ App {..}
  where
    cmdParser =
      hsubparser $
        mconcat
          [ command "gen" $ info genCommand $ progDesc "Generate and serve the static site",
            command "new" $ info newCommand $ progDesc "Create a new zettel",
            command "open" $ info openCommand $ progDesc "Open the local static site",
            command "search" $ info searchCommand $ progDesc "Search zettels and print their path",
            command "query" $ info queryCommand $ progDesc "Query the zettelkasten in JSON"
          ]
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
          (const $ OpenCommand $ Some R.Route_ImpulseStatic)
          (switch (long "search" <> help "Open the search page"))
          <|> fmap
            (OpenCommand . Some . R.Route_Zettel)
            (strOption (long "slug" <> help "Open the zettel HTML page" <> metavar "SLUG"))
    queryCommand = do
      cached <- switch (long "cached" <> help "Use cached zettelkasten graph (faster)")
      query <-
        fmap
          Left
          ( fmap
              (Some . flip Q.ZettelQuery_ZettelByID connDummy)
              (option zettelIDReader (long "id"))
              <|> fmap
                (\x -> Some $ Q.ZettelQuery_ZettelsByTag (mkDefaultTagQuery x) connDummy def)
                (many (mkTagPattern <$> option str (long "tag" <> short 't')))
              <|> option queryReader (long "uri" <> short 'u')
          )
          <|> fmap
            Right
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
    genCommand = do
      serve <- hostPortOption
      watch <-
        switch
          ( long "watch"
              <> short 'w'
              <> help "Watch for changes and regenerate"
          )
      pure $ Gen $ GenCommand {..}

    zettelIDReader :: ReadM ZettelID
    zettelIDReader =
      eitherReader $ first show . parseZettelID . toText
    queryReader :: ReadM (Some Q.ZettelQuery)
    queryReader =
      eitherReader $ \(toText -> s) -> case URI.mkURI s of
        Right uri ->
          maybe (Left "Not a valid query") Right $
            Q.parseQueryLink uri
        Left e ->
          Left $ displayException e
    dateReader :: ReadM DateMayTime
    dateReader =
      maybeReader (parseDateMayTime . toText)
    -- We don't care about connections in the CLI, but the query requires one -
    -- so pass a dummy value.
    connDummy = C.OrdinaryConnection
