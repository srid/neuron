{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Widget where

import Data.Time
import Neuron.Zettelkasten.Zettel.Meta (zettelDateFormat)
import Reflex.Dom.Core
import Relude

-- | <time> element
elTime :: DomBuilder t m => Day -> m ()
elTime t = do
  -- cf. https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time#Attributes
  let formatted = toText $ formatTime defaultTimeLocale zettelDateFormat t
  elAttr "time" ("datetime" =: formatted) $ text formatted
