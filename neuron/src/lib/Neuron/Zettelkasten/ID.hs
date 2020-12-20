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
    Slug,
    indexZid,
    parseZettelID,
    allowedSpecialChars,
    idParser,
    idParser',
    getZettelID,
    zettelIDSourceFileName,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    FromJSONKey (fromJSONKey),
    FromJSONKeyFunction (FromJSONKeyTextParser),
    ToJSON (toJSON),
    ToJSONKey (toJSONKey),
  )
import Data.Aeson.Types (toJSONKeyText)
import Neuron.Reader.Type (ZettelFormat, zettelFormatToExtension)
import Relude
import System.FilePath (splitExtension, takeFileName)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple (Parser, parse)
import qualified Text.Show

type Slug = Text

newtype ZettelID = ZettelID {unZettelID :: Text}
  deriving (Show, Ord, Eq, Generic)

indexZid :: ZettelID
indexZid = ZettelID "index"

instance Show InvalidID where
  show (InvalidIDParseError s) =
    "Invalid Zettel ID: " <> toString s

instance ToJSON ZettelID where
  toJSON = toJSON . unZettelID

instance FromJSON ZettelID where
  parseJSON = fmap ZettelID . parseJSON

instance ToJSONKey ZettelID where
  toJSONKey = toJSONKeyText unZettelID

instance FromJSONKey ZettelID where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case parseZettelID s of
      Right v -> pure v
      Left e -> fail $ show e

zettelIDSourceFileName :: ZettelID -> ZettelFormat -> FilePath
zettelIDSourceFileName zid fmt =
  toString (unZettelID zid <> ext)
  where
    ext = zettelFormatToExtension fmt

---------
-- Parser
---------

data InvalidID = InvalidIDParseError Text
  deriving (Eq, Generic, ToJSON, FromJSON)

parseZettelID :: Text -> Either InvalidID ZettelID
parseZettelID =
  first InvalidIDParseError . parse idParser "parseZettelID"

-- | Characters, aside from alpha numeric characters, to allow in IDs
allowedSpecialChars :: [Char]
allowedSpecialChars =
  [ '_',
    '-',
    '.',
    -- Whitespace is essential for title IDs
    -- This gets replaced with underscope in ID slug
    ' ',
    -- Allow some puctuation letters that are common in note titles
    ',',
    ';',
    '(',
    ')',
    ':',
    '"',
    '\''
  ]

idParser :: Parser ZettelID
idParser = idParser' allowedSpecialChars

idParser' :: String -> Parser ZettelID
idParser' cs = do
  s <- M.some $ M.alphaNumChar <|> M.choice (M.char <$> cs)
  pure $ ZettelID $ toText s

-- | Parse the ZettelID if the given filepath is a zettel.
getZettelID :: ZettelFormat -> FilePath -> Maybe ZettelID
getZettelID fmt fp = do
  let (name, ext) = splitExtension $ takeFileName fp
  guard $ zettelFormatToExtension fmt == toText ext
  rightToMaybe $ parseZettelID $ toText name
