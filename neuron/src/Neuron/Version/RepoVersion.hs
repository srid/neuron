{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Neuron git repo version
--
-- This module is loaded in ghcid and cabal new-repl. However, nix-build will
-- have it overwritten by the then `git describe` (see default.nix).
module Neuron.Version.RepoVersion
  ( version,
  )
where

import Development.GitRev (gitDescribe, gitDirty)
import Relude

version :: Text
version = toText $ $(gitDescribe) <> bool "" "-dirty" $(gitDirty)
