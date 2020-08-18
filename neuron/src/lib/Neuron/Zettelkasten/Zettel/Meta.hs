{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Zettel.Meta
  ( Meta (..),
    -- Date type
    DateMayTime,
    mkDateMayTime,
    getDay,
    -- Date formatting
    dateTimeFormat,
    formatDay,
    formatLocalTime,
    formatDateMayTime,
    -- Date parsing
    parseDateMayTime
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.TagTree (Tag)
import Data.Time
import Data.YAML
import Relude

-- | YAML metadata in a zettel markdown file
data Meta = Meta
  { title :: Maybe Text,
    tags :: Maybe [Tag],
    -- | Creation day
    date :: Maybe DateMayTime,
    -- | List in the z-index
    unlisted :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromYAML Meta where
  parseYAML =
    withMap "Meta" $ \m ->
      Meta
        <$> m .:? "title"
        -- "keywords" is an alias for "tags"
        <*> liftA2 (<|>) (m .:? "tags") (m .:? "keywords")
        <*> m .:? "date"
        <*> m .:? "unlisted"

-- NOTE: Not using this instance because it generates "tags: null" when tags is
-- Nothing.
-- instance ToYAML Meta where
--   toYAML Meta {..} =
--     mapping
--       [ "title" .= title,
--         "tags" .= tags,
--         "date" .= date
--       ]

-- | Like `Day` but with optional time.
newtype DateMayTime = DateMayTime {unDateMayTime :: (Day, Maybe TimeOfDay)}
  deriving (Eq, Show, Generic, Ord, ToJSON, FromJSON)

instance FromYAML DateMayTime where
  parseYAML =
    parseDateMayTime <=< parseYAML @Text

instance ToYAML DateMayTime where
  toYAML =
    toYAML . formatDateMayTime

mkDateMayTime :: Either Day LocalTime -> DateMayTime
mkDateMayTime =
  DateMayTime . \case
    Left day ->
      (day, Nothing)
    Right datetime ->
      localDay &&& Just . localTimeOfDay $ datetime

getDay :: DateMayTime -> Day
getDay = fst . unDateMayTime

formatDateMayTime :: DateMayTime -> Text
formatDateMayTime (DateMayTime (day, mtime)) =
  maybe (formatDay day) (formatLocalTime . LocalTime day) mtime

formatDay :: Day -> Text
formatDay = formatTime' dateFormat

formatLocalTime :: LocalTime -> Text
formatLocalTime = formatTime' dateTimeFormat

parseDateMayTime :: (MonadFail m, Alternative m) => Text -> m DateMayTime
parseDateMayTime (toString -> s) = do
  fmap mkDateMayTime $
    fmap Left (parseTimeM False defaultTimeLocale dateFormat s)
      <|> fmap Right (parseTimeM False defaultTimeLocale dateTimeFormat s)

dateFormat :: String
dateFormat = "%Y-%m-%d"

dateTimeFormat :: String
dateTimeFormat = "%Y-%m-%dT%H:%M"

-- | Like `formatTime` but with default time locale and returning Text
formatTime' :: FormatTime t => String -> t -> Text
formatTime' s = toText . formatTime defaultTimeLocale s
