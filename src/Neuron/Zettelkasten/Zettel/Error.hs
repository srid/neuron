{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Error where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Tagged (untag)
import qualified Data.Text as T
import Neuron.Markdown (ZettelParseError)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Zettel (MissingZettel)
import Relude

-- | All possible errors for a given zettel ID
--
-- The first two constructors correspond to pre-resolved state, the later two to
-- post-resolved zettels (either unparsed, or parsed but with bad queries)
data ZettelIssue
  = ZettelIssue_Error ZettelError
  | ZettelIssue_MissingLinks
      ( -- Slug of the zettel which has 1 or more missing wiki-links
        Slug,
        -- List of missing wiki-links
        NonEmpty MissingZettel
      )
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ZettelError
  = -- | A zettel ID may refer one of several zettel files
    ZettelError_AmbiguousID (NonEmpty FilePath)
  | -- | A slug is shared more than one zettel file
    ZettelError_AmbiguousSlug Slug
  | -- | The zettel file content is malformed
    ZettelError_ParseError (Slug, ZettelParseError)
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

splitZettelIssues ::
  Map ZettelID ZettelIssue ->
  ([(ZettelID, ZettelError)], [(ZettelID, (Slug, NonEmpty MissingZettel))])
splitZettelIssues m =
  lefts &&& rights $
    flip fmap (Map.toList m) $ \(zid, issue) ->
      case issue of
        ZettelIssue_Error err -> Left (zid, err)
        ZettelIssue_MissingLinks x -> Right (zid, x)

zettelErrorText :: ZettelError -> Text
zettelErrorText = \case
  ZettelError_AmbiguousID filePaths ->
    "Multiple zettels have the same ID: " <> T.intercalate ", " (toText <$> toList filePaths)
  ZettelError_AmbiguousSlug slug ->
    "Slug '" <> slug <> "' is already used by another zettel"
  ZettelError_ParseError (untag . snd -> parseErr) ->
    parseErr
