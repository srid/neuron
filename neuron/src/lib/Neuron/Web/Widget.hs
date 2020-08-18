{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Widget where

import qualified Data.Text as T
import Neuron.Zettelkasten.Zettel.Meta (DateMayTime, formatDateMayTime, formatDay, getDay)
import Reflex.Dom.Core
import Relude

-- | <time> element
elTime :: DomBuilder t m => DateMayTime -> m ()
elTime t = do
  -- cf. https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time#Attributes
  elAttr "time" ("datetime" =: formatDateMayTime t) $ do
    text $ formatDay $ getDay t

semanticIcon :: DomBuilder t m => Text -> m ()
semanticIcon name = elClass "i" (name <> " icon") blank

elLinkGoogleFonts :: DomBuilder t m => [Text] -> m ()
elLinkGoogleFonts fs =
  let fsEncoded = T.intercalate "|" $ T.replace " " "+" <$> fs
      fsUrl = "https://fonts.googleapis.com/css?family=" <> fsEncoded <> "&display=swap"
   in elAttr "link" ("rel" =: "stylesheet" <> "href" =: fsUrl) blank
