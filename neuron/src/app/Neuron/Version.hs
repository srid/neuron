{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Version where

import Data.Tagged (Tagged (..))
import Data.Version (showVersion)
import Neuron.Config.Type
import Paths_neuron (version)
import Relude

-- | Neuron cabal library version
neuronVersion :: NeuronVersion
neuronVersion = Tagged $ toText $ showVersion version
