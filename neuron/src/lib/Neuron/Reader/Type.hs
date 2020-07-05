{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.Type
  ( ZettelReader,
    ZettelParseError,
    ZettelFormat (..),
    zettelFormatToExtension,
  )
where

import Data.Aeson
import Data.Tagged
import Neuron.Zettelkasten.Zettel.Meta
import Relude hiding (readEither, show)
import Text.Pandoc.Definition (Pandoc)
import Text.Read
import Prelude (show)

type ZettelReader = FilePath -> Text -> Either ZettelParseError (Maybe Meta, Pandoc)

type ZettelParseError = Tagged "ZettelParserError" Text

data ZettelFormat
  = ZettelFormat_Markdown
  | ZettelFormat_Org
  deriving (Eq, Ord, Generic)

instance FromJSON ZettelFormat where
  parseJSON =
    either fail pure . readEither <=< parseJSON

instance ToJSON ZettelFormat where
  toJSON =
    toJSON . show

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
