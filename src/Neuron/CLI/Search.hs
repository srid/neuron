{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Neuron.CLI.Search
  ( interactiveSearch,
  )
where

import Neuron.CLI.Types (MonadApp, SearchBy (SearchByContent, SearchByTitle), SearchCommand (..), getNotesDir)
import Relude
import System.Process
#if defined(mingw32_HOST_OS)
import Paths_neuron
#else
import System.Which (staticWhich)
#endif

neuronSearchScript :: IO FilePath
scriptName :: String

scriptName = "neuron-search"
#if defined(mingw32_HOST_OS)
neuronSearchScript = getDataFileName scriptName
#else
neuronSearchScript = return $(staticWhich "neuron-search")
#endif

searchScriptArgs :: SearchCommand -> [String]
searchScriptArgs SearchCommand {..} =
  let extensionPattern = "**/*{.md}"
      searchByArgs =
        case searchBy of
          SearchByTitle -> ["(^# )|(^title: )", "2", extensionPattern]
          SearchByContent -> ["", "2", extensionPattern]
      editArg =
        bool "echo" "$EDITOR" searchEdit
   in searchByArgs <> [editArg]

interactiveSearch :: (MonadIO m, MonadApp m) => SearchCommand -> m ()
interactiveSearch searchCmd = do
  notesDir <- getNotesDir
  searchScriptPath <- liftIO neuronSearchScript
  liftIO $ execScript searchScriptPath $ notesDir : searchScriptArgs searchCmd
  where
    execScript scriptPath args =
      -- We must use the low-level execvp (via the unix package's `executeFile`)
      -- here, such that the new process replaces the current one. fzf won't work
      -- otherwise.
      callProcess "bash" $ scriptPath : args
