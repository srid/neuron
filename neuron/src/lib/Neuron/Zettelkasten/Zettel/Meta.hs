{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Zettel.Meta
  ( Meta (..),
    dateTimeFormat,
    formatZettelDate,
    formatZettelDateAsDay,
    parseZettelDate,
    parseZettelDay,
    DateMayTime,
  )
where

import Data.TagTree (Tag)
import Data.Time
import Data.YAML
import Relude

type DateMayTime = Either Day LocalTime

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
        <*> (liftA2 (<|>) (m .:? "tags") (m .:? "keywords"))
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

instance FromYAML DateMayTime where
  parseYAML =
    parseZettelDate <=< parseYAML @Text

instance ToYAML DateMayTime where
  toYAML =
    toYAML . formatZettelDate

formatZettelDate :: DateMayTime -> Text
formatZettelDate =
  toText . \case
    Left day -> formatTime defaultTimeLocale dateFormat day
    Right localtime -> formatTime defaultTimeLocale dateTimeFormat localtime

formatZettelDateAsDay :: DateMayTime -> Text
formatZettelDateAsDay =
  toText . formatTime defaultTimeLocale dateFormat . \case
    Left day -> day
    Right localtime -> localDay localtime

parseZettelDate :: (MonadFail m, Alternative m) => Text -> m DateMayTime
parseZettelDate (toString -> s) =
  Left <$> parseTimeM False defaultTimeLocale dateFormat s <|> Right <$> parseTimeM False defaultTimeLocale dateTimeFormat s

parseZettelDay :: MonadFail m => Text -> m Day
parseZettelDay =
  parseTimeM False defaultTimeLocale dateFormat . toString

dateFormat :: String
dateFormat = "%Y-%m-%d"

dateTimeFormat :: String
dateTimeFormat = "%Y-%m-%dT%H:%M"
