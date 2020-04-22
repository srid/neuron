{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util where

import Relude
import Text.URI (URI, mkURI)

parseURIWith :: (URI -> Either e a) -> Text -> Either (Either Text e) a
parseURIWith f s =
  case mkURI s of
    Left uriError ->
      Left $ Left $ toText $ displayException uriError
    Right uri ->
      first Right $ f uri
