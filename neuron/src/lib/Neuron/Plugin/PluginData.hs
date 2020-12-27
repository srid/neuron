{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module should be remain as independent as possible, as it is imported
-- by Zettel.hs, and we want to avoid cyclic dependencies.
module Neuron.Plugin.PluginData where

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

-- Every zettel is associated with custom data for each plugin.
data PluginData a where
  -- | Tag and parent zettel associated with a directory zettel
  PluginData_DirTree :: PluginData (Tag, Maybe ZettelID)

deriveArgDict ''PluginData
deriveJSONGADT ''PluginData
deriveGEq ''PluginData
deriveGShow ''PluginData
deriveGCompare ''PluginData
