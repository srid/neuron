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

import Neuron.CLI.Rib
import Relude
import System.Posix.Process
import System.Which

neuronSearchScript :: FilePath
neuronSearchScript = $(staticWhich "neuron-search")

searchScriptArgs :: SearchCommand -> [String]
searchScriptArgs SearchCommand {..} =
  let searchByArgs =
        case searchBy of
          SearchByTitle -> ["(^# )|(^title: )", "2"]
          SearchByContent -> ["", "2"]
      editArg =
        bool "echo" "$EDITOR" searchEdit
   in searchByArgs <> [editArg]

interactiveSearch :: FilePath -> SearchCommand -> IO ()
interactiveSearch notesDir searchCmd =
  execScript neuronSearchScript $ notesDir : searchScriptArgs searchCmd
  where
    execScript scriptPath args =
      -- We must use the low-level execvp (via the unix package's `executeFile`)
      -- here, such that the new process replaces the current one. fzf won't work
      -- otherwise.
      void $ executeFile scriptPath False args Nothing
