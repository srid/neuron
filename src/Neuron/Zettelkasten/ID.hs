{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ID
  ( ZettelID (..),
    InvalidID (..),
    zettelIDDay,
    zettelIDText,
    parseZettelID,
    parseZettelID',
    idParser,
    mkZettelID,
    zettelNextId,
    zettelIDSourceFileName,
    zettelPath,
    customIDParser,
  )
where

import Data.Aeson (ToJSON (toJSON))
import qualified Data.Text as T
import Data.Time
import Development.Shake (Action)
import Lucid
import Relude
import qualified Rib
import System.Directory (listDirectory)
import System.FilePath
import qualified System.FilePattern as FP
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
  deriving (Eq, Show, Ord)

instance Show InvalidID where
  show (InvalidIDParseError s) =
    "Invalid Zettel ID: " <> toString s

instance ToJSON ZettelID where
  toJSON = toJSON . zettelIDText

instance ToHtml ZettelID where
  toHtmlRaw = toHtml
  toHtml = toHtml . zettelIDText

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

zettelIDDay :: ZettelID -> Maybe Day
zettelIDDay = \case
  ZettelCustomID _ -> Nothing
  ZettelDateID day _ ->
    Just day

zettelPath :: ZettelID -> Action FilePath
zettelPath zid = do
  notesDir <- Rib.ribInputDir
  pure $ notesDir </> zettelIDSourceFileName zid

zettelNextId :: Day -> Action ZettelID
zettelNextId day = do
  inputDir <- Rib.ribInputDir
  let dayS = toString $ formatDay day
  zettelFiles <- liftIO $ listDirectory inputDir
  let nums :: [Int] =
        sort $ mapMaybe readMaybe
          $ catMaybes
          $ mapMaybe (fmap listToMaybe . FP.match (dayS <> "*.md")) zettelFiles
  case fmap last (nonEmpty nums) of
    Just lastNum ->
      pure $ ZettelDateID day (lastNum + 1)
    Nothing ->
      pure $ ZettelDateID day 1

---------
-- Parser
---------

data InvalidID = InvalidIDParseError Text
  deriving (Eq)

parseZettelID :: Text -> ZettelID
parseZettelID =
  either (error . show) id . parseZettelID'

parseZettelID' :: Text -> Either InvalidID ZettelID
parseZettelID' =
  first InvalidIDParseError . parse idParser "parseZettelID"

idParser :: Parser ZettelID
idParser =
  M.try (fmap (uncurry ZettelDateID) dayParser)
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
mkZettelID :: FilePath -> ZettelID
mkZettelID fp =
  let (name, _) = splitExtension $ takeFileName fp
   in parseZettelID $ toText name
