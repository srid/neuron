{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Zettel.Meta
  ( Meta (..),
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

--getMeta :: MMark -> Maybe Meta
--getMeta src = do
--  val <- projectYaml src
--  case fromJSON val of
--    Error e -> error $ "JSON error: " <> toText e
--    Success v -> pure v

instance FromYAML Meta where
  parseYAML =
    withMap "Meta" $ \m ->
      Meta
        <$> m .: "title"
        <*> m .:? "tags"
        <*> m .:? "date"

instance FromYAML Day where
  parseYAML =
    parseTimeM False defaultTimeLocale "%Y-%m-%d" . toString
      <=< parseYAML @Text
