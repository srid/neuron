{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettel ID
module Neuron.Zettelkasten.ID
  ( ZettelID (..),
    Connection (..),
    ZettelConnection,
    zettelIDDate,
    parseZettelID,
    mkZettelID,
    zettelNextIdForToday,
    zettelIDSourceFileName,
  )
where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import Data.Time
import Lucid
import Path
import Relude
import System.Directory (listDirectory)
import qualified System.FilePattern as FP
import Text.Printf

-- Short Zettel ID encoding `Day` and a numeric index (on that day).
--
-- Based on https://old.reddit.com/r/Zettelkasten/comments/fa09zw/shorter_zettel_ids/
newtype ZettelID = ZettelID {unZettelID :: Text}
  deriving (Eq, Show, Ord, ToJSON)

instance ToHtml ZettelID where
  toHtmlRaw = toHtml
  toHtml = toHtml . unZettelID

zettelIDSourceFileName :: ZettelID -> Text
zettelIDSourceFileName zid = unZettelID zid <> ".md"

-- TODO: sync/DRY with zettelNextIdForToday
zettelIDDate :: ZettelID -> Day
zettelIDDate =
  parseTimeOrError False defaultTimeLocale "%y%W%a"
    . toString
    . uncurry mappend
    . second (dayFromIndex . readMaybe . toString)
    . (T.dropEnd 1 &&& T.takeEnd 1)
    . T.dropEnd 2
    . unZettelID
  where
    dayFromIndex :: Maybe Int -> Text
    dayFromIndex = \case
      Just n ->
        case n of
          1 -> "Mon"
          2 -> "Tue"
          3 -> "Wed"
          4 -> "Thu"
          5 -> "Fri"
          6 -> "Sat"
          7 -> "Sun"
          _ -> error "> 7"
      Nothing ->
        error "Bad day"

zettelNextIdForToday :: Path b Dir -> IO ZettelID
zettelNextIdForToday inputDir = ZettelID <$> do
  zIdPartial <- dayIndex . toText . formatTime defaultTimeLocale "%y%W%a" <$> getCurrentTime
  zettelFiles <- listDirectory $ toFilePath $ inputDir
  let nums :: [Int] = sort $ catMaybes $ fmap readMaybe $ catMaybes $ catMaybes $ fmap (fmap listToMaybe . FP.match (toString zIdPartial <> "*.md")) zettelFiles
  case fmap last (nonEmpty nums) of
    Just lastNum ->
      pure $ zIdPartial <> toText @String (printf "%02d" $ lastNum + 1)
    Nothing ->
      pure $ zIdPartial <> "01"
  where
    dayIndex =
      T.replace "Mon" "1"
        . T.replace "Tue" "2"
        . T.replace "Wed" "3"
        . T.replace "Thu" "4"
        . T.replace "Fri" "5"
        . T.replace "Sat" "6"
        . T.replace "Sun" "7"

-- TODO: Actually parse and validate
parseZettelID :: Text -> ZettelID
parseZettelID = ZettelID

-- | Extract ZettelID from the zettel's filename or path.
mkZettelID :: Path Rel File -> ZettelID
mkZettelID fp = either (error . toText . displayException) id $ do
  (name, _) <- splitExtension $ filename fp
  pure $ ZettelID $ toText $ toFilePath name

type ZettelConnection = (Connection, ZettelID)

-- | Represent the connection between zettels
data Connection
  = -- | A folgezettel points to a zettel that is conceptually a part of the
    -- parent zettel.
    Folgezettel
  | -- | Any other ordinary connection (eg: "See also")
    OrdinaryConnection
  deriving (Eq, Ord, Show, Enum, Bounded)
