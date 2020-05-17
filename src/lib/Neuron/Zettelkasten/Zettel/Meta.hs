{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Zettel.Meta
  ( Meta (..),
    zettelDateFormat
  )
where

import Data.TagTree (Tag)
import Data.Time
import Data.YAML
import Relude

-- | YAML metadata in a zettel markdown file
data Meta = Meta
  { title :: Text,
    tags :: Maybe [Tag],
    -- | Creation day
    date :: Maybe Day
  }
  deriving (Eq, Show, Generic)

instance FromYAML Meta where
  parseYAML =
    withMap "Meta" $ \m ->
      Meta
        <$> m .: "title"
        <*> m .:? "tags"
        <*> m .:? "date"

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
    parseTimeM False defaultTimeLocale zettelDateFormat . toString
      <=< parseYAML @Text

instance ToYAML Day where
  toYAML =
    toYAML . toText . formatTime defaultTimeLocale zettelDateFormat

-- | The format in which we decode and encode zettel dates.
zettelDateFormat :: String
zettelDateFormat = "%Y-%m-%d"
