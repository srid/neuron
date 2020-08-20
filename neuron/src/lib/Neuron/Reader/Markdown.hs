{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.Markdown
  ( parseMarkdown,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Blocks as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Inlines as CM
import qualified Commonmark.Pandoc as CP
import qualified Commonmark.Tag
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens (TokType (..))
import Control.Monad.Combinators (manyTill)
import Control.Monad.Except
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import qualified Data.YAML as YAML
import Neuron.Orphans ()
import Neuron.Reader.Type (ZettelParseError, ZettelReader)
import Neuron.Zettelkasten.Zettel.Meta (Meta)
import Relude hiding (show, traceShowId)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Parsec as P
import Text.Show

-- | Parse Markdown document, along with the YAML metadata block in it.
--
-- We are not using the Pandoc AST "metadata" field (as it is not clear whether
-- we actually need it), and instead directly decoding the metadata as Haskell
-- object.
parseMarkdown :: ZettelReader
parseMarkdown fn s = do
  (metaVal, markdown) <-
    first (Tagged . ("Unable to determine YAML region: " <>)) $
      partitionMarkdown fn s
  v <-
    first (Tagged . toText . show) $
      commonmarkPandocWith neuronSpec fn markdown
  meta <- traverse (parseMeta fn) metaVal
  pure (meta, Pandoc mempty $ B.toList (CP.unCm v))
  where
    -- NOTE: HsYAML parsing is rather slow due to its use of DList.
    -- See https://github.com/haskell-hvr/HsYAML/issues/40
    parseMeta :: FilePath -> Text -> Either ZettelParseError Meta
    parseMeta n v = do
      let raw = encodeUtf8 v
      let mkError (loc, emsg) =
            Tagged $ toText $ n <> ":" <> YAML.prettyPosWithSource loc raw " error" <> emsg
      first mkError $ YAML.decode1 raw
    -- Like commonmarkWith, but parses directly into the Pandoc AST.
    commonmarkPandocWith ::
      CM.SyntaxSpec (Either P.ParseError) (CP.Cm () B.Inlines) (CP.Cm () B.Blocks) ->
      String ->
      Text ->
      Either P.ParseError (CP.Cm () B.Blocks)
    commonmarkPandocWith spec n =
      join . CM.commonmarkWith spec n

-- | Identify metadata block at the top, and split it from markdown body.
partitionMarkdown :: FilePath -> Text -> Either Text (Maybe Text, Text)
partitionMarkdown fn =
  parse (M.try splitP <|> fmap (Nothing,) M.takeRest) fn
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
    CE.HasMath il,
    CE.HasDefinitionList il bl,
    CE.HasDiv bl,
    CE.HasQuoted il
  ) =>
  CM.SyntaxSpec m il bl
neuronSpec =
  mconcat
    [ wrappedLinkSpec angleBracketLinkP,
      wikiLinkNewP,
      gfmExtensionsSansEmoji,
      CE.fancyListSpec,
      CE.footnoteSpec,
      CE.mathSpec,
      CE.smartPunctuationSpec,
      CE.definitionListSpec,
      CE.attributesSpec,
      CE.rawAttributeSpec,
      CE.fencedDivSpec,
      CM.defaultSyntaxSpec {CM.syntaxBlockSpecs = defaultBlockSpecsSansRawHtml}
    ]
  where
    -- Emoji extension introduces ghcjs linker issues
    gfmExtensionsSansEmoji =
      CE.strikethroughSpec
        <> CE.pipeTableSpec
        <> CE.autoIdentifiersSpec
        <> CE.taskListSpec

-- | Convert the given wrapped link to a `B.Link`.
wrappedLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  P.ParsecT [CM.Tok] (CM.IPState m) (StateT Commonmark.Tag.Enders m) [CM.Tok] ->
  CM.SyntaxSpec m il bl
wrappedLinkSpec linkP =
  mempty
    { CM.syntaxInlineParsers = [pLink linkP]
    }
  where
    pLink ::
      (Monad m, CM.IsInline il) =>
      P.ParsecT [CM.Tok] (CM.IPState m) (StateT Commonmark.Tag.Enders m) [CM.Tok] ->
      CM.InlineParser m il
    pLink p = P.try $ do
      x <- p
      let url = CM.untokenize x
          title = ""
      pure $! CM.link url title $ CM.str url

angleBracketLinkP :: Monad m => P.ParsecT [CM.Tok] s m [CM.Tok]
angleBracketLinkP = do
  void $ symbol '<'
  -- NOTE: Intentionally be lenient to support `<z:zettels?t...>` style
  -- queries. FIXME: Should fail on `</foo>` though (HTML end tags). TODO:
  -- Add unit tests before modifying this matching any further.
  x <- some (noneOfToks [Symbol '>', Spaces, UnicodeSpace, LineEnd])
  void $ symbol '>'
  pure x

wikiLinkNewP ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
wikiLinkNewP =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pLink = P.try $ do
      -- [[[foo]]] is folgezettel; [[foo]] is cf.
      url <-
        P.try (CM.untokenize <$> wikiLinkP 3)
          -- TODO: refactor and add test coverage
          -- hackish way to patch the URI with a new flag (without really parsing and re-rendering it).
          <|> (\s -> if isJust (T.find (== '?') s) then s <> "&cf" else s <> "?cf") . CM.untokenize <$> wikiLinkP 2
      -- NOTE: Still have to inject final URI into Link node (think org parser, doing the same)
      let title = ""
      pure $! CM.link url title $ CM.str url

wikiLinkP :: Monad m => Int -> P.ParsecT [CM.Tok] s m [CM.Tok]
wikiLinkP n = do
  void $ M.count n $ symbol '['
  x <- some (noneOfToks [Symbol ']', Spaces, UnicodeSpace, LineEnd])
  void $ M.count n $ symbol ']'
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
