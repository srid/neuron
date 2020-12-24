{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Version where

import Data.Version (showVersion)
import Paths_neuron (version)
import Relude

-- | Neuron cabal library version
neuronVersion :: Text
neuronVersion = toText $ showVersion version
