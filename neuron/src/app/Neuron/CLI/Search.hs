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

import Neuron.CLI.Types (MonadApp, SearchBy (SearchByContent, SearchByTitle), SearchCommand (..), getNotesDir)
import Relude
import System.Posix.Process (executeFile)
import System.Which (staticWhich)

neuronSearchScript :: FilePath
neuronSearchScript = $(staticWhich "neuron-search")

searchScriptArgs :: SearchCommand -> [String]
searchScriptArgs SearchCommand {..} =
  let extensionPattern = "/*{.md}"
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
  liftIO $ execScript neuronSearchScript $ notesDir : searchScriptArgs searchCmd
  where
    execScript scriptPath args =
      -- We must use the low-level execvp (via the unix package's `executeFile`)
      -- here, such that the new process replaces the current one. fzf won't work
      -- otherwise.
      void $ executeFile scriptPath False args Nothing
