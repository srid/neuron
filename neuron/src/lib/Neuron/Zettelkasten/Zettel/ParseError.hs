{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Neuron.Zettelkasten.Zettel.ParseError where

import Data.Aeson (FromJSON, ToJSON)
import Relude hiding (show)

data ZettelParseError
  = ZettelParseError_InvalidMarkdown Text
  | ZettelParseError_InvalidOrg Text
  | ZettelParseError_InvalidYAML Text
  | ZettelParseError_PartitionError Text
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show ZettelParseError where
  show = \case
    ZettelParseError_InvalidMarkdown e ->
      show e
    ZettelParseError_InvalidOrg e ->
      show e
    ZettelParseError_InvalidYAML e ->
      toString e
    ZettelParseError_PartitionError e ->
      "Unable to determine YAML region: " <> toString e
