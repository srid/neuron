{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Zettel.Meta
  ( Meta (..),
    formatZettelDate,
    parseZettelDate,
  )
where

import Data.TagTree (Tag)
import Data.Time
import Data.YAML
import Relude

-- | YAML metadata in a zettel markdown file
data Meta = Meta
  { title :: Maybe Text,
    tags :: Maybe [Tag],
    -- | Creation day
    date :: Maybe Day,
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

instance FromYAML Day where
  parseYAML =
    parseZettelDate <=< parseYAML @Text

instance ToYAML Day where
  toYAML =
    toYAML . formatZettelDate

-- | The format in which we decode and encode zettel dates.
zettelDateFormat :: String
zettelDateFormat = "%Y-%m-%d"

formatZettelDate :: Day -> Text
formatZettelDate =
  toText . formatTime defaultTimeLocale zettelDateFormat

parseZettelDate :: MonadFail m => Text -> m Day
parseZettelDate =
  parseTimeM False defaultTimeLocale zettelDateFormat . toString
