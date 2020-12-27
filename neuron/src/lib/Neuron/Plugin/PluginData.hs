{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.PluginData where

-- TODO: Consolidate with the `a` of `Plugin a`, and move it there

import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.TagTree (Tag)
import Neuron.Zettelkasten.ID (ZettelID)
import Relude

data PluginData a where
  PluginData_DirectoryZettel :: PluginData (Tag, Maybe ZettelID)

deriveArgDict ''PluginData
deriveJSONGADT ''PluginData
deriveGEq ''PluginData
deriveGShow ''PluginData
deriveGCompare ''PluginData
