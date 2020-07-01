{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Format where

import Data.Aeson (FromJSON, ToJSON)
import Relude

data ZettelFormat
  = ZettelFormat_Markdown
  | ZettelFormat_Org
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

zettelFormatToExtension :: ZettelFormat -> Text
zettelFormatToExtension = \case
  ZettelFormat_Markdown -> ".md"
  ZettelFormat_Org -> ".org"

extensionToZettelFormat :: Text -> Maybe ZettelFormat
extensionToZettelFormat = \case
  ".md" -> Just ZettelFormat_Markdown
  ".org" -> Just ZettelFormat_Org
  _ -> Nothing
