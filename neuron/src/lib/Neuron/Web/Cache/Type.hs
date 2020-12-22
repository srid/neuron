{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Cache.Type where

import Data.Aeson (FromJSON, ToJSON)
import Neuron.Config.Type (Config)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Zettel (ZettelError)
import Relude

data ReadMode
  = ReadMode_Direct Config
  | ReadMode_Cached
  deriving (Eq, Show)

data NeuronCache = NeuronCache
  { _neuronCache_graph :: ZettelGraph,
    _neuronCache_errors :: Map ZettelID ZettelError,
    _neuronCache_config :: Config,
    _neuronCache_neuronVersion :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
