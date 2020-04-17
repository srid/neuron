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
    literalPattern,
    tagPatternToText,
    tagMatch,
    isSubTag,
    isStrictSubTag,
  )
where

import Data.Aeson
import Relude
import Relude.Extra.Foldable1
import System.FilePath
import System.FilePattern as FilePattern

newtype Tag = Tag {tagToText :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

instance Semigroup Tag where
  Tag t <> Tag t' = Tag (toText $ toString t </> toString t')
  sconcat tags = Tag (toText $ foldl1' (</>) $ fmap (toString . tagToText) tags)

newtype TagPattern = TagPattern {toFilePattern :: FilePattern}
  deriving (Eq, Show)

instance Semigroup TagPattern where
  TagPattern p <> TagPattern p' = TagPattern (p </> p')

tagPatternToText :: TagPattern -> Text
tagPatternToText = toText . toFilePattern

literalPattern :: Tag -> TagPattern
literalPattern = TagPattern . toString . tagToText

tagMatch :: TagPattern -> Tag -> Bool
tagMatch (TagPattern pat) (Tag tag) = pat ?== toString tag

isSubTag :: Tag -> Tag -> Bool
isSubTag tag tag' = tagMatch (literalPattern tag <> TagPattern "**") tag'

isStrictSubTag :: Tag -> Tag -> Bool
isStrictSubTag tag tag' = tagMatch (literalPattern tag <> TagPattern "*" <> TagPattern "**") tag'
