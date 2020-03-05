{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Self.Zettelkasten.Type where

import Relude
import Self.Zettelkasten.ID
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
