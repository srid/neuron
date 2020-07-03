{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Config.Orphans where

import Dhall (FromDhall)
import Neuron.Config.Type

deriving instance FromDhall Config
