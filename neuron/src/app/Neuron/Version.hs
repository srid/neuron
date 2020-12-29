{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Version where

import Data.Tagged (Tagged (..))
import Data.Version (showVersion)
import Neuron.Web.Route (NeuronVersion)
import Paths_neuron (version)
import Relude (ToText (toText), ($))

-- | Neuron cabal library version
neuronVersion :: NeuronVersion
neuronVersion = Tagged $ toText $ showVersion version
