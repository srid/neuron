{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.Type
  ( ZettelReader,
    ZettelFormat (..),
    zettelFormatToExtension,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Neuron.Zettelkasten.Zettel.Meta
import Relude
import Text.Pandoc.Definition (Pandoc)
import Text.Read
import Prelude (show)

type ZettelReader = FilePath -> Text -> Either Text (Maybe Meta, Pandoc)

data ZettelFormat
  = ZettelFormat_Markdown
  | ZettelFormat_Org
  deriving (Eq, Ord, Generic, FromJSON, ToJSON)

instance Show ZettelFormat where
  show ZettelFormat_Markdown = "markdown"
  show ZettelFormat_Org = "org"

instance Read ZettelFormat where
  readPrec =
    choice
      [ do
          Ident "org" <- lexP
          pure ZettelFormat_Org,
        do
          Ident "markdown" <- lexP
          pure ZettelFormat_Markdown
      ]

zettelFormatToExtension :: ZettelFormat -> Text
zettelFormatToExtension = \case
  ZettelFormat_Markdown -> ".md"
  ZettelFormat_Org -> ".org"
