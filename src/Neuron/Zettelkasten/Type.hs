{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Type where

import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Meta as Z
import Relude
import Text.MMark (MMark)

data Zettel
  = Zettel
      { zettelID :: ZettelID,
        zettelTitle :: Text,
        zettelContent :: MMark
      }

instance Eq Zettel where
  (==) = (==) `on` zettelID

instance Ord Zettel where
  compare = compare `on` zettelID

hasTag :: Text -> Zettel -> Bool
hasTag t =
  isJust . (find (== t) <=< Z.tags <=< Z.getMeta) . zettelContent
