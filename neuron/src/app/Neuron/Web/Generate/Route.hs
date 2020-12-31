{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Static generation of routes
module Neuron.Web.Generate.Route where

import Control.Monad.Except (liftEither, runExcept)
import qualified Network.URI.Encode as E
import Neuron.Web.Route (Route (..), routeHtmlPath)
import Relude
import Text.URI (URI, mkURI)
import qualified Text.URI as URI

data BaseUrlError
  = BaseUrlNotAbsolute
  deriving (Eq, Show)

instance Exception BaseUrlError

-- | Make an absolute URI for a route, given a base URL.
routeUri :: HasCallStack => URI -> Route a -> URI
routeUri baseUrl r = either (error . toText . displayException) id $
  runExcept $ do
    let -- Use `E.encode` to deal with unicode code points, as mkURI will fail on them.
        -- This is necessary to support non-ascii characters in filenames
        relUrl = toText . E.encode . toString $ routeHtmlPath r
    uri <- liftEither $ mkURI relUrl
    case URI.relativeTo uri baseUrl of
      Nothing -> liftEither $ Left $ toException BaseUrlNotAbsolute
      Just x -> pure x
