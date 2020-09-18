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
    unsafeMkZettelID,
    indexZid,
    parseZettelID,
    idParser,
    getZettelID,
    zettelIDSourceFileName,
  )
where

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Text as T
import Neuron.Reader.Type (ZettelFormat, zettelFormatToExtension)
import Relude
import System.FilePath
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple
import qualified Text.Show

data ZettelID = ZettelID
  { -- | Slug must be unique
    zettelIDSlug :: Text,
    -- | Actual ID used by the user, inside `[[..]]`
    zettelIDRaw :: Text
  }
  deriving (Show, Ord, Generic)

-- | Make ZettelID from raw text.
--
-- Assumes that input text is already validated for allowed characters.
unsafeMkZettelID :: Text -> ZettelID
unsafeMkZettelID s =
  let slug = T.intercalate "_" $ T.splitOn " " s
   in ZettelID slug s

indexZid :: ZettelID
indexZid = unsafeMkZettelID "index"

instance Eq ZettelID where
  (==) (ZettelID a _) (ZettelID b _) = a == b

instance Show InvalidID where
  show (InvalidIDParseError s) =
    "Invalid Zettel ID: " <> toString s

instance ToJSON ZettelID where
  toJSON = toJSON . zettelIDRaw

instance FromJSON ZettelID where
  parseJSON = fmap unsafeMkZettelID . parseJSON

instance ToJSONKey ZettelID where
  toJSONKey = toJSONKeyText zettelIDSlug

instance FromJSONKey ZettelID where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case parseZettelID s of
      Right v -> pure v
      Left e -> fail $ show e

zettelIDSourceFileName :: ZettelID -> ZettelFormat -> FilePath
zettelIDSourceFileName zid fmt = toString $ zettelIDRaw zid <> zettelFormatToExtension fmt

---------
-- Parser
---------

data InvalidID = InvalidIDParseError Text
  deriving (Eq, Generic, ToJSON, FromJSON)

parseZettelID :: Text -> Either InvalidID ZettelID
parseZettelID =
  first InvalidIDParseError . parse idParser "parseZettelID"

idParser :: Parser ZettelID
idParser = do
  s <-
    M.some $
      M.alphaNumChar
        <|> M.char '_'
        <|> M.char '-'
        <|> M.char '.'
        <|> M.char ' '
  pure $ unsafeMkZettelID (toText s)

-- | Parse the ZettelID if the given filepath is a zettel.
getZettelID :: ZettelFormat -> FilePath -> Maybe ZettelID
getZettelID fmt fp = do
  let (name, ext) = splitExtension $ takeFileName fp
  guard $ zettelFormatToExtension fmt == toText ext
  rightToMaybe $ parseZettelID $ toText name
