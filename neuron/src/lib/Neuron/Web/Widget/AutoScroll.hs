{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Widget.AutoScroll where

import Data.Tagged
import Reflex.Dom.Core hiding ((&))
import Relude

type AutoScroll = Tagged "autoScroll" (Maybe Text)

-- | Hidden element to scroll to.
-- cf. https://stackoverflow.com/a/49968820/55246
marker :: DomBuilder t m => Text -> Int -> m ()
marker elemId offsetPx = do
  let style = "position: absolute; top: " <> show offsetPx <> "px; left: 0"
  elAttr "div" ("id" =: elemId <> "style" =: style) blank

-- | Place this at the bottom of the page, to have it auto scroll to the marker.
--
-- FIXME: This may not scroll sufficiently if the images in the zettel haven't
-- loaded (thus the browser doesn't known the final height yet.)
script :: DomBuilder t m => Text -> m ()
script markerId = do
  let s =
        "if (window.scrollY == 0) { document.getElementById(\"" <> markerId <> "\").scrollIntoView({behavior: \"smooth\", block: \"start\"}); }"
  el "script" $ text s
