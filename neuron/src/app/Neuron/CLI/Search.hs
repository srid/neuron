{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Search
  ( interactiveSearch,
  )
where

import Data.FileEmbed (embedOneStringFileOf)
import qualified Data.Text as Text
import Data.Text.IO (hPutStr)
import Development.Shake (Action)
import Neuron.CLI.Rib
  ( SearchBy (SearchByContent, SearchByTitle),
    SearchCommand (..),
  )
import Neuron.Config.Type (Config, getZettelFormats)
import Neuron.Reader.Type (ZettelFormat (ZettelFormat_Org), zettelFormatToExtension)
import Relude
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Posix.Files (setFileMode)
import System.Posix.Process (executeFile)

searchScript :: Text
searchScript = $(embedOneStringFileOf ["./src-bash/neuron-search", "./neuron/src-bash/neuron-search"])

searchScriptArgs :: (NonEmpty ZettelFormat) -> SearchCommand -> [String]
searchScriptArgs formats SearchCommand {..} =
  let extensionPattern = "/*{" <> (Text.unpack $ Text.intercalate "," $ toList $ zettelFormatToExtension <$> formats) <> "}"
      searchByArgs =
        case searchBy of
          SearchByTitle -> ["(^# )|(^title: )", "2", extensionPattern]
          SearchByContent -> ["", "2", extensionPattern]
      editArg =
        bool "echo" "$EDITOR" searchEdit
   in searchByArgs <> [editArg]

interactiveSearch :: FilePath -> SearchCommand -> Config -> Action ()
interactiveSearch notesDir searchCmd config =
  do
    zettelFormats <- getZettelFormats config
    if searchBy searchCmd == SearchByTitle && ZettelFormat_Org `elem` toList zettelFormats
      then fail "search is not supported for .org files"
      else liftIO $ do
        asExecutableScript "neuron-search" searchScript $ \scriptFile -> do
          execScript scriptFile $ notesDir : searchScriptArgs zettelFormats searchCmd
  where
    asExecutableScript k s f =
      withSystemTempFile k $ \fp hdl -> do
        hPutStr hdl s
        hClose hdl
        setFileMode fp 0o700
        f fp
    execScript scriptPath args =
      -- We must use the low-level execvp (via the unix package's `executeFile`)
      -- here, such that the new process replaces the current one. fzf won't work
      -- otherwise.
      void $ executeFile scriptPath False args Nothing
