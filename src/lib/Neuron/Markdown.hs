{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Markdown where

import qualified Commonmark as CM
import qualified Commonmark.Blocks as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Inlines as CM
import qualified Commonmark.Pandoc as CP
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens (TokType (..))
import Control.Monad.Combinators (manyTill)
import Control.Monad.Except
import qualified Data.YAML as YAML
import Relude
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import qualified Text.Parsec as P
import qualified Text.URI as URI

-- | Parse Markdown document, along with the YAML metadata block in it.
--
-- We are not using the Pandoc AST "metadata" field (as it is not clear whether
-- we actually need it), and instead directly decoding the metadata as Haskell
-- object.
parseMarkdown :: forall meta. YAML.FromYAML meta => FilePath -> Text -> Either Text (meta, Pandoc)
parseMarkdown fn s = do
  (metaVal, markdown) <- partitionMarkdown fn s
  v <- commonmarkPandocWith neuronSpec fn markdown
  meta <- parseMeta fn metaVal
  pure (meta, Pandoc mempty $ B.toList (CP.unCm v))
  where
    -- NOTE: HsYAML parsing is rather slow due to its use of DList.
    -- See https://github.com/haskell-hvr/HsYAML/issues/40
    parseMeta :: FilePath -> Text -> Either Text meta
    parseMeta n v = do
      let raw = encodeUtf8 v
      let mkError (loc, emsg) = toText $ n <> ":" <> YAML.prettyPosWithSource loc raw " error" <> emsg
      first mkError $ YAML.decode1 raw
    -- Like commonmarkWith, but parses directly into the Pandoc AST.
    commonmarkPandocWith ::
      CM.SyntaxSpec (Either P.ParseError) (CP.Cm () B.Inlines) (CP.Cm () B.Blocks) ->
      String ->
      Text ->
      Either Text (CP.Cm () B.Blocks)
    commonmarkPandocWith spec n =
      first show . join . CM.commonmarkWith spec n

-- | Identify metadata block at the top, and split it from markdown body.
partitionMarkdown :: FilePath -> Text -> Either Text (Text, Text)
partitionMarkdown fn =
  parse splitP fn
  where
    separatorP :: Parser ()
    separatorP =
      void $ M.string "---" <* M.eol
    splitP :: Parser (Text, Text)
    splitP = do
      separatorP
      a <- toText <$> manyTill M.anySingle (M.try $ M.eol *> separatorP)
      b <- M.takeRest
      pure (a, b)

data MarkdownLink = MarkdownLink
  { markdownLinkText :: Text,
    markdownLinkUri :: URI.URI
  }
  deriving (Eq, Ord)

extractAutoLinks :: Pandoc -> [MarkdownLink]
extractAutoLinks = W.query go
  where
    go :: B.Inline -> [MarkdownLink]
    go = \case
      (B.Link _attr [B.Str linkText] (url, _title))
        | linkText == url -> maybeToList $ do
          uri <- URI.mkURI url
          pure $ MarkdownLink linkText uri
      _ -> []

-- | Return the link in the given inline.
pandocLinkInline :: B.Inline -> Maybe MarkdownLink
pandocLinkInline = \case
  (B.Link _attr [B.Str linkText] (url, _title)) -> do
    uri <- URI.mkURI url
    pure $ MarkdownLink linkText uri
  _ -> Nothing

-- | Like `pandocLinkInline` but expects the link to be on a paragraph of its
-- own.
pandocLinkBlock :: B.Block -> Maybe MarkdownLink
pandocLinkBlock = \case
  B.Para [B.Link _attr [B.Str linkText] (url, _title)] -> do
    uri <- URI.mkURI url
    pure $ MarkdownLink linkText uri
  _ -> Nothing

getFirstParagraphText :: Pandoc -> Maybe [B.Inline]
getFirstParagraphText = listToMaybe . W.query go
  where
    go :: B.Block -> [[B.Inline]]
    go = \case
      B.Para inlines ->
        [inlines]
      _ ->
        []

neuronSpec ::
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
    CE.HasMath il
  ) =>
  CM.SyntaxSpec m il bl
neuronSpec =
  mconcat
    [ angleBracketLinkSpec,
      gfmExtensionsSansEmoji,
      CE.footnoteSpec,
      CE.mathSpec,
      CE.smartPunctuationSpec,
      CE.attributesSpec,
      CE.rawAttributeSpec,
      CM.defaultSyntaxSpec {CM.syntaxBlockSpecs = defaultBlockSpecsSansRawHtml}
    ]
  where
    -- Emoji extension introduces ghcjs linker issues
    gfmExtensionsSansEmoji =
      CE.strikethroughSpec
        <> CE.pipeTableSpec
        <> CE.autoIdentifiersSpec
        <> CE.taskListSpec

-- | Unconditionally treat <...> as a `B.Link`.
angleBracketLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
angleBracketLinkSpec =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink :: (Monad m, CM.IsInline il) => CM.InlineParser m il
    pLink = P.try $ do
      x <- angleBracketLinkP
      let url = CM.untokenize x
          title = show x
      pure $! CM.link url title $ CM.str url

angleBracketLinkP :: Monad m => P.ParsecT [CM.Tok] s m [CM.Tok]
angleBracketLinkP = do
  void $ symbol '<'
  x <- some (noneOfToks [Symbol '>', Spaces, UnicodeSpace, LineEnd])
  void $ symbol '>'
  pure x

-- rawHtmlSpec eats angle bracket links as html tags
defaultBlockSpecsSansRawHtml :: (Monad m, CM.IsBlock il bl) => [CM.BlockSpec m il bl]
defaultBlockSpecsSansRawHtml =
  [ CM.indentedCodeSpec,
    CM.fencedCodeSpec,
    CM.blockQuoteSpec,
    CM.atxHeadingSpec,
    CM.setextHeadingSpec,
    CM.thematicBreakSpec,
    CM.listItemSpec (CM.bulletListMarker <|> CM.orderedListMarker),
    myRawHtmlSpec,
    CM.attributeSpec
  ]

-- | Like `CM.rawHtmlSpec` but lets zettel style links pass through.
myRawHtmlSpec ::
  (Monad m, CM.IsBlock il bl) =>
  CM.BlockSpec m il bl
myRawHtmlSpec =
  -- TODO: Ideally we should use a more restrictive parsers; one that allows known safe HTML tags
  -- Although, this prevents the user from naming their zettels say "div.md"
  CM.rawHtmlSpec {CM.blockStart = P.notFollowedBy angleBracketLinkP >> CM.blockStart CM.rawHtmlSpec}
