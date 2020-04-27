{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.TagTree
  ( Tag (..),
    TagPattern (unTagPattern),
    TagNode (..),
    mkTagPattern,
    tagMatch,
    tagMatchAny,
    tagTree,
    foldTagTree,
    constructTag,
  )
where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.PathTree (annotatePathsWith, foldSingleParentsWith, mkTreeFromPaths)
import qualified Data.Text as T
import Data.Tree (Forest)
import Relude
import System.FilePattern
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple

-- | Tag metadata field in Zettel notes
newtype Tag = Tag {unTag :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

--------------
-- Tag Pattern
---------------

-- | Glob-based pattern matching of tags
--
-- Eg.: "foo/**" matches both "foo/bar/baz" and "foo/baz"
newtype TagPattern = TagPattern {unTagPattern :: FilePattern}
  deriving (Eq, Show, ToJSON)

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

-----------
-- Tag Tree
-----------

-- | A tag like "foo/bar/baz" is split into three nodes "foo", "bar" and "baz."
newtype TagNode = TagNode {unTagNode :: Text}
  deriving (Eq, Show, Ord, ToJSON)

deconstructTag :: HasCallStack => Tag -> NonEmpty TagNode
deconstructTag (Tag s) =
  either error id $ parse tagParser (toString s) s
  where
    tagParser :: Parser (NonEmpty TagNode)
    tagParser =
      nodeParser `sepBy1` M.char '/'
    nodeParser :: Parser TagNode
    nodeParser =
      TagNode . toText <$> M.some (M.anySingleBut '/')

constructTag :: NonEmpty TagNode -> Tag
constructTag (fmap unTagNode . toList -> nodes) =
  Tag $ T.intercalate "/" nodes

-- | Construct a tree from a list of tags
tagTree :: ann ~ Natural => Map Tag ann -> Forest (TagNode, ann)
tagTree tags =
  fmap (annotatePathsWith $ countFor tags)
    $ mkTreeFromPaths
    $ fmap (toList . deconstructTag)
    $ Map.keys tags
  where
    countFor tags' path =
      fromMaybe 0 $ Map.lookup (constructTag path) tags'

foldTagTree :: ann ~ Natural => Forest (TagNode, ann) -> Forest (NonEmpty TagNode, ann)
foldTagTree tree =
  foldSingleParentsWith foldNodes <$> fmap (fmap (first (:| []))) tree
  where
    foldNodes (parent, 0) (child, count) = Just (parent <> child, count)
    foldNodes _ _ = Nothing
