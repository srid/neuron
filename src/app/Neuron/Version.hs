{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Version where

import Data.Version (Version, makeVersion, parseVersion, showVersion)
import Relude
import Text.ParserCombinators.ReadP (readP_to_S)

-- This must be same as what cabal file uses.
-- We are not using Paths_neuron, because it causes cyclic references in nix static build.
--
version :: Version
version = makeVersion [0, 5, 1, 0]

-- | Neuron cabal library version
neuronVersion :: Text
neuronVersion = toText $ showVersion version

-- | Check if `neuronVersion` is older than the given version
olderThan :: Text -> Bool
olderThan s =
  case reverse (readP_to_S parseVersion (toString s)) of
    (v2, _) : _ -> version < v2
    x -> error $ "Version parse error: " <> show x
