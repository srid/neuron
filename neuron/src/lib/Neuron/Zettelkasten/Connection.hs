{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Connection where

import Data.Aeson (FromJSON, ToJSON)
import Relude hiding (show)
import Text.Show

-- | Represent the connection between zettels
data Connection
  = -- | A folgezettel points to a zettel that is conceptually a *part* of the
    -- parent zettel.
    Folgezettel
  | -- | Any other ordinary connection (eg: "See also")
    OrdinaryConnection
  deriving (Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

instance Semigroup Connection where
  -- A folgezettel link trumps all other kinds in that zettel.
  Folgezettel <> _ = Folgezettel
  _ <> Folgezettel = Folgezettel
  OrdinaryConnection <> OrdinaryConnection = OrdinaryConnection

instance Show Connection where
  show = \case
    Folgezettel -> "folgezettel"
    OrdinaryConnection -> "cf"
