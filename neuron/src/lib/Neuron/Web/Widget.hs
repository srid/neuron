{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Widget where

import Data.Time
import Neuron.Zettelkasten.Zettel.Meta (formatZettelDate)
import Reflex.Dom.Core
import Relude

-- | <time> element
elTime :: DomBuilder t m => Day -> m ()
elTime t = do
  let s = formatZettelDate t
  -- cf. https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time#Attributes
  elAttr "time" ("datetime" =: s) $ text s

semanticIcon :: DomBuilder t m => Text -> m ()
semanticIcon name = elClass "i" (name <> " icon") blank
