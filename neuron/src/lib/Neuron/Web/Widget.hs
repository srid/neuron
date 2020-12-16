{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Widget where

import qualified Data.Text as T
import Data.Time.DateMayTime (DateMayTime, formatDateMayTime, formatDay, getDay)
import Reflex.Dom.Core
import Relude

-- | <time> element
elTime :: DomBuilder t m => DateMayTime -> m ()
elTime t = do
  -- cf. https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time#Attributes
  elAttr "time" ("datetime" =: formatDateMayTime t) $ do
    text $ formatDay $ getDay t

-- | A pre element with scrollbar
elPreOverflowing :: DomBuilder t m => m a -> m a
elPreOverflowing w =
  elAttr "pre" ("style" =: "overflow: auto") w

semanticIcon :: DomBuilder t m => Text -> m ()
semanticIcon name = elClass "i" (name <> " icon") blank

elLinkGoogleFonts :: DomBuilder t m => [Text] -> m ()
elLinkGoogleFonts fs =
  let fsEncoded = T.intercalate "|" $ T.replace " " "+" <$> fs
      fsUrl = "https://fonts.googleapis.com/css?family=" <> fsEncoded <> "&display=swap"
   in elAttr "link" ("rel" =: "stylesheet" <> "href" =: fsUrl) blank

-- Prevent this element from appearing in Google search results
-- https://developers.google.com/search/reference/robots_meta_tag#data-nosnippet-attr
elNoSnippetSpan :: DomBuilder t m => Map Text Text -> m a -> m a
elNoSnippetSpan attrs = elAttr "span" ("data-nosnippet" =: "" <> attrs)

elVisible :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m a
elVisible visible w = do
  elDynAttr "span" (ffor visible $ bool ("style" =: "display: none;") mempty) w