{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Markdown
  ( parseMarkdown,
    highlightStyle,
    lookupZettelMeta,
    NeuronSyntaxSpec,
    ZettelParser,
    ZettelParseError,
    ZettelMeta,
  )
where

import Clay (Css, (?))
import qualified Clay as C
import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Inlines as CM
import qualified Commonmark.Pandoc as CP
import Control.Monad.Combinators (manyTill)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON)
import Data.Default
import Data.Tagged (Tagged (..))
import qualified Data.YAML as Y
import Data.YAML.ToJSON ()
import Relude hiding (show, traceShowId)
import qualified Relude.Extra.Map as M
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple (Parser, parse)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Parsec as P
import Text.Show (Show (show))

-- | A zettel parser is a function that parses the given body of text, and
-- returns both the Pandoc AST and the associated metadata (as Aeson Value)
type ZettelParser = FilePath -> Text -> Either ZettelParseError (ZettelMeta, Pandoc)

type ZettelParseError = Tagged "ZettelParserError" Text

newtype ZettelMeta = ZettelMeta {unZettelMeta :: Maybe Aeson.Value}
  deriving
    ( Eq,
      Ord,
      Show
    )
  deriving newtype
    ( ToJSON,
      FromJSON
    )

instance Default ZettelMeta where
  def = ZettelMeta Nothing

zettelMetaFromYamlFrontmatter :: Maybe (Y.Node Y.Pos) -> ZettelMeta
zettelMetaFromYamlFrontmatter myaml =
  ZettelMeta $ patchMdYaml . toJSON <$> myaml
  where
    patchMdYaml =
      -- Some zettelkasten apps like Zettlr use "keywords" for tags
      withJsonAlias ("keywords", "tags")

lookupZettelMeta :: forall a. FromJSON a => Text -> ZettelMeta -> Maybe a
lookupZettelMeta k (ZettelMeta mobj) = do
  Aeson.Object m <- mobj
  Aeson.Success v <- Aeson.fromJSON @a <$> M.lookup k m
  pure v

-- | Parse Markdown document, along with the YAML metadata block in it.
--
-- We are not using the Pandoc AST "metadata" field (as it is not clear whether
-- we actually need it), and instead directly decoding the metadata as Haskell
-- object.
parseMarkdown ::
  NeuronSyntaxSpec m il bl =>
  CM.SyntaxSpec m il bl ->
  ZettelParser
parseMarkdown extraSpec fn s = do
  (metaVal, markdown) <-
    first (Tagged . ("Unable to determine YAML region: " <>)) $
      partitionMarkdown fn s
  v <-
    first (Tagged . toText . show) $
      commonmarkPandocWith (extraSpec <> neuronSpec) fn markdown
  meta <- zettelMetaFromYamlFrontmatter <$> traverse (parseYaml @(Y.Node Y.Pos) fn) metaVal
  let doc = Pandoc mempty $ B.toList (CP.unCm v)
  pure (meta, doc)

-- | Treat `alias` as an alias to `target`, but only if `target` doesn't already exist
withJsonAlias :: (Text, Text) -> Aeson.Value -> Aeson.Value
withJsonAlias (alias, target) = \case
  x@(Aeson.Object m) -> fromMaybe x $ do
    guard $ not $ M.member target m
    kw <- M.lookup alias m
    pure $ Aeson.Object (M.insert target kw m)
  x -> x

-- NOTE: HsYAML parsing is rather slow due to its use of DList.
-- See https://github.com/haskell-hvr/HsYAML/issues/40
parseYaml :: Y.FromYAML a => FilePath -> Text -> Either ZettelParseError a
parseYaml n (encodeUtf8 -> v) = do
  let mkError (loc, emsg) =
        Tagged $ toText $ n <> ":" <> Y.prettyPosWithSource loc v " error" <> emsg
  first mkError $ Y.decode1 v

-- Like commonmarkWith, but parses directly into the Pandoc AST.
commonmarkPandocWith ::
  NeuronSyntaxSpec m il bl =>
  CM.SyntaxSpec m il bl ->
  FilePath ->
  Text ->
  m bl
commonmarkPandocWith spec fn s =
  join $ CM.commonmarkWith spec fn s

-- | Identify metadata block at the top, and split it from markdown body.
partitionMarkdown :: FilePath -> Text -> Either Text (Maybe Text, Text)
partitionMarkdown =
  parse (M.try splitP <|> fmap (Nothing,) M.takeRest)
  where
    separatorP :: Parser ()
    separatorP =
      void $ M.string "---" <* M.eol
    splitP :: Parser (Maybe Text, Text)
    splitP = do
      separatorP
      a <- toText <$> manyTill M.anySingle (M.try $ M.eol *> separatorP)
      b <- M.takeRest
      pure (Just a, b)

-- | Like `NeuronSyntaxSpec'` but specialized to Pandoc
type NeuronSyntaxSpec m il bl =
  ( NeuronSyntaxSpec' m il bl,
    m ~ Either P.ParseError,
    bl ~ CP.Cm () B.Blocks
  )

type NeuronSyntaxSpec' m il bl =
  ( Monad m,
    CM.IsBlock il bl,
    CM.IsInline il,
    Typeable m,
    Typeable il,
    Typeable bl,
    CE.HasEmoji il,
    CE.HasStrikethrough il,
    CE.HasPipeTable il bl,
    CE.HasTaskList il bl,
    CM.ToPlainText il,
    CE.HasFootnote il bl,
    CE.HasMath il,
    CE.HasDefinitionList il bl,
    CE.HasDiv bl,
    CE.HasQuoted il,
    CE.HasSpan il,
    HasHighlight il
  )

neuronSpec ::
  NeuronSyntaxSpec' m il bl =>
  CM.SyntaxSpec m il bl
neuronSpec =
  -- TODO: Move the bulk of markdown extensions to a neuron extension
  mconcat
    [ highlightSpec,
      gfmExtensionsSansEmoji,
      CE.fancyListSpec,
      CE.footnoteSpec,
      CE.mathSpec,
      CE.smartPunctuationSpec,
      CE.definitionListSpec,
      CE.attributesSpec,
      CE.rawAttributeSpec,
      CE.fencedDivSpec,
      CE.bracketedSpanSpec,
      CE.autolinkSpec,
      CM.defaultSyntaxSpec,
      -- as the commonmark documentation states, pipeTableSpec should be placed after
      -- fancyListSpec and defaultSyntaxSpec to avoid bad results when non-table lines
      CE.pipeTableSpec
    ]
  where
    -- Emoji extension introduces ghcjs linker issues
    gfmExtensionsSansEmoji =
      CE.strikethroughSpec
        <> CE.autoIdentifiersSpec
        <> CE.taskListSpec

-- TODO: Move to a plugin
highlightSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il, HasHighlight il) =>
  CM.SyntaxSpec m il bl
highlightSpec =
  mempty
    { CM.syntaxFormattingSpecs =
        [ CM.FormattingSpec '=' True True Nothing (Just highlight) '='
        ]
    }

class HasHighlight a where
  highlight :: a -> a

instance HasHighlight (CM.Html a) where
  highlight x = CM.htmlInline "mark" (Just x)

instance
  (HasHighlight i, Monoid i) =>
  HasHighlight (CM.WithSourceMap i)
  where
  highlight x = (highlight <$> x) <* CM.addName "highlight"

instance HasHighlight (CP.Cm a B.Inlines) where
  highlight ils =
    B.spanWith attr <$> ils
    where
      attr = ("", ["highlight"], [])

highlightStyle :: Css
highlightStyle = do
  -- In lieu of <mark>
  ".highlight" ? do
    C.backgroundColor C.yellow
