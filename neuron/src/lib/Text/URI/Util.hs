{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.URI.Util where

import Control.Monad.Catch (MonadThrow)
import qualified Network.URI.Encode as E
import Relude
import qualified Text.URI as URI

mkURILenient :: MonadThrow m => Text -> m URI.URI
mkURILenient =
  URI.mkURI . toText . E.encode . toString

getQueryParam :: URI.RText 'URI.QueryKey -> URI.URI -> Maybe Text
getQueryParam k uri =
  listToMaybe $
    catMaybes $
      flip fmap (URI.uriQuery uri) $ \case
        URI.QueryFlag _ -> Nothing
        URI.QueryParam key (URI.unRText -> val) ->
          if key == k
            then Just val
            else Nothing

hasQueryFlag :: URI.RText 'URI.QueryKey -> URI.URI -> Bool
hasQueryFlag k uri =
  fromMaybe False $
    listToMaybe $
      catMaybes $
        flip fmap (URI.uriQuery uri) $ \case
          URI.QueryFlag key ->
            if key == k
              then Just True
              else Nothing
          _ -> Nothing
