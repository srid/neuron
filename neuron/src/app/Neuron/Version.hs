{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Version where

import Data.Tagged (Tagged (..))
import Data.Version (showVersion)
import Neuron.Frontend.Route.Data.Types (NeuronVersion)
import Paths_neuron (version)
import Relude (ToText (toText), ($))

-- | Neuron cabal library version
neuronVersion :: NeuronVersion
neuronVersion = Tagged $ toText $ showVersion version
