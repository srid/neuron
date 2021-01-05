{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module should be remain as independent as possible, as it is imported
-- by Zettel.hs, and we want to avoid cyclic dependencies.
--
-- See also Route.Data.Types where PluginZettelRouteData is defined.
module Neuron.Plugin.PluginData where

import Data.Aeson
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

data DirZettel = DirZettel
  { -- | What to tag this directory zettel.
    -- We expect the arity here to be 1-2. 1 for the simplest case; and 2, if
    -- both Foo/ and Foo.md exists, with the later being positioned *elsewhere*
    -- in the tree, with its own parent directory.
    _dirZettel_tags :: Set Tag,
    -- | The tag used by its child zettels (directories and files)
    _dirZettel_childrenTag :: Tag,
    -- | The zettel associated with the parent directory.
    _dirZettel_dirParent :: Maybe ZettelID
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Every zettel is associated with custom data for each plugin.
-- TODO: Rename to PluginZettelData
data PluginZettelData a where
  -- | Tag (and another optional tag, if the user's directory zettel is
  -- positioned in a *different* directory) and parent zettel associated with a
  -- directory zettel
  PluginZettelData_DirTree :: PluginZettelData DirZettel
  PluginZettelData_NeuronIgnore :: PluginZettelData ()

deriveArgDict ''PluginZettelData
deriveJSONGADT ''PluginZettelData
deriveGEq ''PluginZettelData
deriveGShow ''PluginZettelData
deriveGCompare ''PluginZettelData
