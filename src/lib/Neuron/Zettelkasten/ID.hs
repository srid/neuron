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
    zettelIDText,
    parseZettelID,
    parseZettelID',
    idParser,
    mkZettelID,
    zettelIDSourceFileName,
    customIDParser,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import qualified Data.Text as T
import Data.Time
import Relude
import System.FilePath
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple
import Text.Printf
import qualified Text.Show

data ZettelID
  = -- | Short Zettel ID encoding `Day` and a numeric index (on that day).
    ZettelDateID Day Int
  | -- | Arbitrary alphanumeric ID.
    ZettelCustomID Text
  deriving (Eq, Show, Ord, Generic)

instance Show InvalidID where
  show (InvalidIDParseError s) =
    "Invalid Zettel ID: " <> toString s

instance FromJSON ZettelID where
  parseJSON x = do
    s <- parseJSON x
    case parseZettelID' s of
      Left e -> fail $ show e
      Right zid -> pure zid

instance ToJSON ZettelID where
  toJSON = toJSON . zettelIDText

zettelIDText :: ZettelID -> Text
zettelIDText = \case
  ZettelDateID day idx ->
    formatDay day <> toText @String (printf "%02d" idx)
  ZettelCustomID s -> s

formatDay :: Day -> Text
formatDay day =
  subDay $ toText $ formatTime defaultTimeLocale "%y%W%a" day
  where
    subDay =
      T.replace "Mon" "1"
        . T.replace "Tue" "2"
        . T.replace "Wed" "3"
        . T.replace "Thu" "4"
        . T.replace "Fri" "5"
        . T.replace "Sat" "6"
        . T.replace "Sun" "7"

zettelIDSourceFileName :: ZettelID -> FilePath
zettelIDSourceFileName zid = toString $ zettelIDText zid <> ".md"

---------
-- Parser
---------

data InvalidID = InvalidIDParseError Text
  deriving (Eq)

parseZettelID :: HasCallStack => Text -> ZettelID
parseZettelID =
  either (error . show) id . parseZettelID'

parseZettelID' :: Text -> Either InvalidID ZettelID
parseZettelID' =
  first InvalidIDParseError . parse idParser "parseZettelID"

idParser :: Parser ZettelID
idParser =
  M.try (fmap (uncurry ZettelDateID) $ dayParser <* M.eof)
    <|> fmap ZettelCustomID customIDParser

dayParser :: Parser (Day, Int)
dayParser = do
  year <- parseNum 2
  week <- parseNum 2
  dayName <- dayFromIdx =<< parseNum 1
  idx <- parseNum 2
  day <-
    parseTimeM False defaultTimeLocale "%y%W%a" $
      printf "%02d" year <> printf "%02d" week <> dayName
  pure (day, idx)
  where
    parseNum n = readNum =<< M.count n M.digitChar
    readNum = maybe (fail "Not a number") pure . readMaybe
    dayFromIdx :: MonadFail m => Int -> m String
    dayFromIdx idx =
      maybe (fail "Day should be a value from 1 to 7") pure $
        ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"] !!? (idx - 1)

customIDParser :: Parser Text
customIDParser = do
  fmap toText $ M.some $ M.alphaNumChar <|> M.char '_' <|> M.char '-'

-- | Extract ZettelID from the zettel's filename or path.
mkZettelID :: HasCallStack => FilePath -> ZettelID
mkZettelID fp =
  let (name, _) = splitExtension $ takeFileName fp
   in parseZettelID $ toText name
