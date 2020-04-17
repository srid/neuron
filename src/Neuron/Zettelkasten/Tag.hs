{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Tag
  ( Tag (..),
    TagPattern (..),
    tagMatch,
  )
where

import Data.Aeson
import Relude
import System.FilePattern

newtype Tag = Tag {unTag :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype TagPattern = TagPattern {unTagPattern :: FilePattern}
  deriving (Eq, Show)

tagMatch :: TagPattern -> Tag -> Bool
tagMatch (TagPattern pat) (Tag tag) = pat ?== toString tag
