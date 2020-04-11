{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ID
  ( ZettelID (..),
    Connection (..),
    zettelIDDay,
    zettelIDText,
    parseZettelID,
    mkZettelID,
    zettelNextIdForToday,
    zettelIDSourceFileName,
  )
where

import Data.Aeson (ToJSON)
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
import Text.Printf

-- | Short Zettel ID encoding `Day` and a numeric index (on that day).
--
-- Based on https://old.reddit.com/r/Zettelkasten/comments/fa09zw/shorter_zettel_ids/
data ZettelID
  = ZettelDateID Day Int
  | ZettelCustomID Text
  deriving (Eq, Show, Ord, Generic, ToJSON)

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

zettelIDSourceFileName :: ZettelID -> Text
zettelIDSourceFileName zid = zettelIDText zid <> ".md"

zettelIDDay :: ZettelID -> Maybe Day
zettelIDDay = \case
  ZettelCustomID _ -> Nothing
  ZettelDateID day _ ->
    Just day

zettelNextIdForToday :: Action ZettelID
zettelNextIdForToday = do
  inputDir <- Rib.ribInputDir
  day <- utctDay <$> liftIO getCurrentTime
  let dayS = toString $ formatDay day
  zettelFiles <- liftIO $ listDirectory inputDir
  let nums :: [Int] = sort $ catMaybes $ fmap readMaybe $ catMaybes $ catMaybes $ fmap (fmap listToMaybe . FP.match (dayS <> "*.md")) zettelFiles
  case fmap last (nonEmpty nums) of
    Just lastNum ->
      pure $ ZettelDateID day (lastNum + 1)
    Nothing ->
      pure $ ZettelDateID day 1

parseZettelID :: Text -> ZettelID
parseZettelID s =
  either (error . toText . M.errorBundlePretty) id $
    M.parse p "parseZettelID" s
  where
    p =
      fmap (uncurry ZettelDateID) dayParser
        <|> fmap ZettelCustomID customIDParser

dayParser :: M.Parsec Void Text (Day, Int)
dayParser = do
  year <- parseNum 2
  week <- parseNum 2
  dayIdx <- parseNum 1
  idx <- parseNum 2
  day <-
    parseTimeM False defaultTimeLocale "%y%W%a" $
      printf "%02d" year <> printf "%02d" week <> toString (dayName dayIdx)
  pure (day, idx)
  where
    parseNum n = readNum =<< M.count n M.digitChar
    readNum = maybe (fail "Not a number") pure . readMaybe
    dayName :: Int -> Text
    dayName = \case
      1 -> "Mon"
      2 -> "Tue"
      3 -> "Wed"
      4 -> "Thu"
      5 -> "Fri"
      6 -> "Sat"
      7 -> "Sun"
      _ -> error "> 7"

customIDParser :: M.Parsec Void Text Text
customIDParser = do
  fmap toText $ M.some $ M.alphaNumChar <|> M.char '_' <|> M.char '-'

-- | Extract ZettelID from the zettel's filename or path.
mkZettelID :: FilePath -> ZettelID
mkZettelID fp =
  let (name, _) = splitExtension $ takeFileName fp
   in parseZettelID $ toText name

-- | Represent the connection between zettels
data Connection
  = -- | A folgezettel points to a zettel that is conceptually a part of the
    -- parent zettel.
    Folgezettel
  | -- | Any other ordinary connection (eg: "See also")
    OrdinaryConnection
  deriving (Eq, Ord, Show, Enum, Bounded)
