{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Version where

import Data.Version (parseVersion, showVersion)
import Paths_neuron (version)
import Relude
import Text.ParserCombinators.ReadP (readP_to_S)

-- | Neuron cabal library version
neuronVersion :: Text
neuronVersion = toText $ showVersion version

-- | Check if `neuronVersion` is older than the given version
olderThan :: Text -> Bool
olderThan s =
  case reverse (readP_to_S parseVersion (toString s)) of
    (v2, _) : _ -> version < v2
    x -> error $ "Version parse error: " <> show x
