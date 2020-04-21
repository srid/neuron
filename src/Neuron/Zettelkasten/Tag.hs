{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Tag
  ( Tag (..),
    TagPattern (unTagPattern),
    mkTagPattern,
    tagMatch,
    tagMatchAny,
    tagTree,
  )
where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Tree (Forest)
import Neuron.Util.Tree (annotatePathsWith, mkTreeFromPaths)
import Relude
import System.FilePath (splitDirectories)
import System.FilePattern

newtype Tag = Tag {unTag :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

tagTree :: Num a => Map Tag a -> Forest (Text, a)
tagTree tags =
  annotatePathsWith countFor <$> mkTreeFromPaths tagPaths
  where
    tagPaths = tagComponents <$> Map.keys tags
    countFor path =
      let tag = fold $ intersperse "/" path
       in fromMaybe 0 $ Map.lookup (Tag tag) tags
    tagComponents :: Tag -> [Text]
    tagComponents =
      fmap toText
        . splitDirectories
        . toString
        . unTag

newtype TagPattern = TagPattern {unTagPattern :: FilePattern}
  deriving (Eq, Show)

mkTagPattern :: Text -> TagPattern
mkTagPattern =
  TagPattern . toString

tagMatch :: TagPattern -> Tag -> Bool
tagMatch (TagPattern pat) (Tag tag) =
  pat ?== toString tag

tagMatchAny :: [TagPattern] -> Tag -> Bool
tagMatchAny pats tag =
  -- TODO: Use step from https://hackage.haskell.org/package/filepattern-0.1.2/docs/System-FilePattern.html#v:step
  -- for efficient matching.
  any (`tagMatch` tag) pats
