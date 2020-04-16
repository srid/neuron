{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Tag
  ( Tag (..),
    TagPattern (..),
    PatternComponent (..),
    tagSep,
    tagToText,
    patternToText,
    tagParser,
    tagPatternParser,
    parseTag,
    parseTagPattern,
    tagLiteral,
    tagRelativeTo,
    tagMatch,
    isSubTag,
    isStrictSubTag,
  )
where

import Data.Aeson
import qualified Data.List.NonEmpty as NonEmpty
import Neuron.Parser
import Relude
import Relude.Extra.Foldable1
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

data PatternComponent
  = Literal Text
  | Glob
  | GlobStar
  deriving (Eq, Show)

tagSep :: Text
tagSep = "/"

patternComponentToText :: PatternComponent -> Text
patternComponentToText = \case
  Glob -> "*"
  GlobStar -> "**"
  Literal t -> t

tagComponentParser :: Parser Text
tagComponentParser = toText <$> some (M.alphaNumChar <|> M.char '-')

patternComponentParser :: Parser PatternComponent
patternComponentParser =
  M.choice @[]
    [ M.try (GlobStar <$ M.string "**"),
      Glob <$ M.char '*',
      Literal <$> tagComponentParser
    ]

newtype TagPattern = TagPattern
  {patternComponents :: NonEmpty PatternComponent}
  deriving (Eq, Show, Semigroup)

newtype Tag = Tag
  {tagComponents :: NonEmpty Text}
  deriving (Eq, Ord, Show, Semigroup)

instance ToJSON Tag where
  toJSON = toJSON . tagToText

instance FromJSON Tag where
  parseJSON (String tag) = maybe (fail "Failed to parse tags") pure (M.parseMaybe tagParser tag)
  parseJSON _ = fail "Expected string when parsing a tag"

tagToText :: Tag -> Text
tagToText = fold . tagComponents

patternToText :: TagPattern -> Text
patternToText = foldMap1 patternComponentToText . patternComponents

pathParserWith :: Parser c -> Parser (NonEmpty c)
pathParserWith p = NonEmpty.fromList <$> p `M.sepBy1` M.string tagSep

tagParser :: Parser Tag
tagParser = Tag <$> pathParserWith tagComponentParser

tagPatternParser :: Parser TagPattern
tagPatternParser = TagPattern <$> pathParserWith patternComponentParser

parseTag :: Text -> Either Text Tag
parseTag = parse tagParser "parseTag"

parseTagPattern :: Text -> Either Text TagPattern
parseTagPattern = parse tagPatternParser "parseTagPattern"

tagLiteral :: Tag -> TagPattern
tagLiteral = TagPattern . fmap Literal . tagComponents

tagRelativeTo :: TagPattern -> Tag -> Maybe [Text]
tagRelativeTo (TagPattern pat) (Tag components) = go (toList pat) (toList components)
  where
    go [] ps = Just ps
    go (Literal p : ps) (p' : ps')
      | p == p' = go ps ps'
    go (Glob : ps) (_ : ps') = go ps ps'
    go [GlobStar] (_ : _) = Just []
    go (GlobStar : ps) ps' =
      go (Glob : ps) ps'
        <|> go (Glob : GlobStar : ps) ps'
    go _ _ = Nothing

tagMatch :: TagPattern -> Tag -> Bool
tagMatch pat tag = tagRelativeTo pat tag == Just []

isSubTag :: Tag -> Tag -> Bool
isSubTag tag tag' = isJust $ tagRelativeTo (tagLiteral tag') tag

isStrictSubTag :: Tag -> Tag -> Bool
isStrictSubTag tag tag' = isJust $ tagRelativeTo (tagLiteral tag' <> TagPattern [Glob]) tag
