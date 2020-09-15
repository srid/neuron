{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ID
  ( ZettelID (..),
    InvalidID (..),
    parseZettelID,
    idParser,
    getZettelID,
    zettelIDSourceFileName,
    customIDParser,
  )
where

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Neuron.Reader.Type (ZettelFormat, zettelFormatToExtension)
import Relude
import System.FilePath
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple
import qualified Text.Show

newtype ZettelID = ZettelID {unZettelID :: Text}
  deriving (Eq, Show, Ord, Generic)

instance Show InvalidID where
  show (InvalidIDParseError s) =
    "Invalid Zettel ID: " <> toString s

instance FromJSON ZettelID where
  parseJSON x = do
    s <- parseJSON x
    case parseZettelID s of
      Left e -> fail $ show e
      Right zid -> pure zid

instance ToJSONKey ZettelID where
  toJSONKey = toJSONKeyText unZettelID

instance FromJSONKey ZettelID where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case parseZettelID s of
      Right v -> pure v
      Left e -> fail $ show e

instance ToJSON ZettelID where
  toJSON = toJSON . unZettelID

zettelIDSourceFileName :: ZettelID -> ZettelFormat -> FilePath
zettelIDSourceFileName zid fmt = toString $ unZettelID zid <> zettelFormatToExtension fmt

---------
-- Parser
---------

data InvalidID = InvalidIDParseError Text
  deriving (Eq, Generic, ToJSON, FromJSON)

parseZettelID :: Text -> Either InvalidID ZettelID
parseZettelID =
  first InvalidIDParseError . parse idParser "parseZettelID"

idParser :: Parser ZettelID
idParser =
  fmap ZettelID customIDParser

customIDParser :: Parser Text
customIDParser = do
  fmap toText $ M.some $ M.alphaNumChar <|> M.char '_' <|> M.char '-' <|> M.char '.'

-- | Parse the ZettelID if the given filepath is a zettel.
getZettelID :: ZettelFormat -> FilePath -> Maybe ZettelID
getZettelID fmt fp = do
  let (name, ext) = splitExtension $ takeFileName fp
  guard $ zettelFormatToExtension fmt == toText ext
  rightToMaybe $ parseZettelID $ toText name
