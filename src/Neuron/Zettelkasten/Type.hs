{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Type where

import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Meta as Z
import Relude hiding (show)
import Text.MMark (MMark)
import Text.Show (Show (show))

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

instance Show Zettel where
  show Zettel {..} = "Zettel:" <> show zettelID

hasTag :: Text -> Zettel -> Bool
hasTag t =
  isJust . (find (== t) <=< Z.tags <=< Z.getMeta) . zettelContent
