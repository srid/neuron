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
  )
where

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Lucid
import Path
import Relude

-- Short Zettel ID encoding `Day` and a numeric index (on that day).
--
-- Based on https://old.reddit.com/r/Zettelkasten/comments/fa09zw/shorter_zettel_ids/
newtype ZettelID = ZettelID {unZettelID :: Text}
  deriving (Eq, Show, Ord)

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

instance ToHtml ZettelID where
  toHtmlRaw = toHtml
  toHtml = toHtml . unZettelID

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
  deriving (Eq, Show, Enum, Bounded)
