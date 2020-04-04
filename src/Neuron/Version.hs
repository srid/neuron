{-# LANGUAGE TemplateHaskell #-}

-- | Neuron version
--
-- This module is loaded in ghcid and cabal new-repl. However, nix-build will
-- have it overwritten by the then `git describe` (see default.nix).
module Neuron.Version where

import Data.Bool (bool)
import Development.GitRev (gitDescribe, gitDirty)

version :: String
version = $(gitDescribe) <> bool "" "-dirty" $(gitDirty)
