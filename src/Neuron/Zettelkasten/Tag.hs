{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Move TagPattern to a different module, as well as tagTree to its own
-- module.
module Neuron.Zettelkasten.Tag
  ( Tag (..),
    TagPattern (unTagPattern),
    mkTagPattern,
    tagMatch,
    tagMatchAny,
    tagTree,
    foldTagTree,
  )
where

import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Tree (Forest)
import Neuron.Util.Tree (annotatePathsWith, foldTreeOnWith, mkTreeFromPaths)
import Relude
import System.FilePath (splitDirectories)
import System.FilePattern

-- | Tag metadata field in Zettel notes
newtype Tag = Tag {unTag :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | Glob-based pattern matching of tags
--
-- Eg.: "foo/**" matches both "foo/bar/baz" and "foo/baz"
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

-- | Construct a tree from a list of tags
tagTree :: Num a => Map Tag a -> Forest (Text, a)
tagTree tags =
  fmap (annotatePathsWith $ countFor tags)
    $ mkTreeFromPaths
    $ fmap breakTag
    $ Map.keys tags
  where
    countFor tags' path =
      fromMaybe 0 $ Map.lookup (unbreakTag path) tags'
    -- TODO: The breaking/unbreaking mechanism needs to be made more safe
    breakTag :: Tag -> [Text]
    breakTag =
      fmap toText
        . splitDirectories
        . toString
        . unTag
    unbreakTag :: [Text] -> Tag
    unbreakTag = Tag . T.intercalate "/"

foldTagTree :: (Num a, Eq a) => Forest (Text, a) -> Forest (Text, a)
foldTagTree tree =
  fmap (foldTreeOnWith tagDoesNotExist concatRelTags) tree
  where
    concatRelTags (parent, _) (child, count) = (parent <> "/" <> child, count)
    tagDoesNotExist (_, count) = count == 0
