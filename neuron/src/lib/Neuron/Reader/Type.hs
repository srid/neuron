{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.Type
  ( ZettelFormat (..),
    zettelFormatToExtension,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Relude
import Text.Read
import Prelude (show)

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
