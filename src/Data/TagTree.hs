{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.TagTree
  ( Tag (..),
    TagPattern (unTagPattern),
    TagNode (..),
    Query (..),
    mkDefaultTagQuery,
    mkTagPattern,
    mkTagPatternFromTag,
    tagMatch,
    matchTagQuery,
    matchTagQueryMulti,
    tagTree,
    foldTagTree,
    constructTag,
  )
where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import Data.PathTree (annotatePathsWith, foldSingleParentsWith, mkTreeFromPaths)
import qualified Data.Text as T
import Data.Tree (Forest)
import Relude
import System.FilePattern (FilePattern, (?==))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple (Parser, parse)
import Text.Show (Show (show))

-- | A hierarchical tag
--
-- Tag nodes are separated by @/@
newtype Tag = Tag {unTag :: Text}
  deriving (Eq, Ord, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON
    )

--------------
-- Tag Pattern
---------------

-- | A glob-based pattern to match hierarchical tags
--
-- For example, the pattern
--
-- > foo/**
--
-- matches both the following
--
-- > foo/bar/baz
-- > foo/baz
newtype TagPattern = TagPattern {unTagPattern :: FilePattern}
  deriving
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving newtype
    ( ToJSON,
      FromJSON
    )

mkTagPattern :: Text -> TagPattern
mkTagPattern =
  TagPattern . toString

mkTagPatternFromTag :: Tag -> TagPattern
mkTagPatternFromTag (Tag t) =
  TagPattern $ toString t

data Query
  = Query_And (NonEmpty TagPattern)
  | Query_Or (NonEmpty TagPattern)
  | Query_All
  deriving (Eq, Ord, Generic)
  deriving anyclass
    ( ToJSON,
      FromJSON
    )

mkDefaultTagQuery :: [TagPattern] -> Query
mkDefaultTagQuery = maybe Query_All Query_Or . nonEmpty

instance Show Query where
  show = \case
    Query_And (toList -> pats) ->
      toString $ T.intercalate ", and " (fmap (toText . unTagPattern) pats)
    Query_Or (toList -> pats) ->
      toString $ T.intercalate ", or " (fmap (toText . unTagPattern) pats)
    Query_All ->
      "No particular tag"

tagMatch :: TagPattern -> Tag -> Bool
tagMatch (TagPattern pat) (Tag tag) =
  pat ?== toString tag

-- TODO: Use step from https://hackage.haskell.org/package/filepattern-0.1.2/docs/System-FilePattern.html#v:step
-- for efficient matching.
matchTagQuery :: Tag -> Query -> Bool
matchTagQuery t = \case
  Query_And pats -> all (`tagMatch` t) pats
  Query_Or pats -> any (`tagMatch` t) pats
  Query_All -> True

matchTagQueryMulti :: [Tag] -> Query -> Bool
matchTagQueryMulti tags = \case
  Query_And pats -> any (\t -> all (`tagMatch` t) pats) tags
  Query_Or pats -> any (\t -> any (`tagMatch` t) pats) tags
  Query_All -> True

-----------
-- Tag Tree
-----------

-- | An individual component of a hierarchical tag
--
-- The following hierarchical tag,
--
-- > foo/bar/baz
--
-- has three tag nodes: @foo@, @bar@ and @baz@
newtype TagNode = TagNode {unTagNode :: Text}
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON)

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

-- | Construct the tree from a list of hierarchical tags
tagTree :: ann ~ Natural => Map Tag ann -> Forest (TagNode, ann)
tagTree tags =
  fmap (annotatePathsWith $ countFor tags) $
    mkTreeFromPaths $
      toList . deconstructTag
        <$> Map.keys tags
  where
    countFor tags' path =
      fromMaybe 0 $ Map.lookup (constructTag path) tags'

foldTagTree :: ann ~ Natural => Forest (TagNode, ann) -> Forest (NonEmpty TagNode, ann)
foldTagTree tree =
  foldSingleParentsWith foldNodes <$> fmap (fmap (first (:| []))) tree
  where
    foldNodes (parent, 0) (child, count) = Just (parent <> child, count)
    foldNodes _ _ = Nothing
