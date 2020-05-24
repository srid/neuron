{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Type where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.TagTree (Tag, TagPattern (..))
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Theme
import Neuron.Zettelkasten.Zettel
import Relude

-- | Query represents a way to query the zettels list.
--
-- Note that you can only query the individual zettels, and not the graph itself
-- (which needs evaluating queries before building itself).
data Query r where
  Query_ZettelByID :: ZettelID -> Maybe Connection -> Query (Maybe Zettel)
  Query_ZettelsByTag :: [TagPattern] -> Maybe Connection -> ZettelsView -> Query [Zettel]
  Query_Tags :: [TagPattern] -> Query (Map Tag Natural)

deriveJSONGADT ''Query

deriveGEq ''Query

deriveGShow ''Query

deriving instance Show (Query (Maybe Zettel))

deriving instance Show (Query [Zettel])

deriving instance Show (Query (Map Tag Natural))

deriving instance Eq (Query (Maybe Zettel))

deriving instance Eq (Query [Zettel])

deriving instance Eq (Query (Map Tag Natural))
