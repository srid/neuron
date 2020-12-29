{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Error where

import Data.Aeson
import Data.Tagged (untag)
import qualified Data.Text as T
import Neuron.Markdown (ZettelParseError)
import Neuron.Zettelkasten.ID (Slug)
import Neuron.Zettelkasten.Query.Error (QueryResultError)
import Relude

-- | All possible errors for a given zettel ID
data ZettelError
  = -- | The zettel file content is malformed
    ZettelError_ParseError (Slug, ZettelParseError)
  | -- | Some queries in zettel file are incorrect
    ZettelError_QueryResultErrors (Slug, NonEmpty QueryResultError)
  | -- | A zettel ID may refer one of several zettel files
    ZettelError_AmbiguousID (NonEmpty FilePath)
  | -- | A slug is shared more than one zettel file
    ZettelError_AmbiguousSlug Slug
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

zettelErrorText :: ZettelError -> Either Int Text
zettelErrorText = \case
  ZettelError_ParseError (untag . snd -> parseErr) ->
    Right parseErr
  ZettelError_QueryResultErrors queryErrs ->
    Left (length queryErrs)
  ZettelError_AmbiguousID filePaths ->
    Right $ "Multiple zettels have the same ID: " <> T.intercalate ", " (toText <$> toList filePaths)
  ZettelError_AmbiguousSlug slug ->
    Right $ "Slug '" <> slug <> "' is already used by another zettel"
