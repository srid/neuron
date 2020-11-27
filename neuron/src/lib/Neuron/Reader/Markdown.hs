{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.Markdown
  ( parseMarkdown,
    highlightStyle,
  )
where

import Clay (Css, (?))
import qualified Clay as C
import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Inlines as CM
import qualified Commonmark.Pandoc as CP
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens (TokType (..))
import Control.Monad.Combinators (manyTill)
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import qualified Data.YAML as YAML
import Neuron.Orphans ()
import Neuron.Reader.Type (ZettelParseError, ZettelReader)
import Neuron.Zettelkasten.Zettel.Meta (Meta)
import Relude hiding (show, traceShowId)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple (Parser, parse)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Parsec as P
import Text.Show (Show (show))
import Text.URI
  ( QueryParam (QueryParam),
    URI (URI, uriQuery),
    mkPathPiece,
    mkURI,
  )
import qualified Text.URI as URI
import Text.URI.QQ (queryKey, queryValue, scheme)

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
    CE.HasQuoted il,
    CE.HasSpan il,
    HasHighlight il
  ) =>
  CM.SyntaxSpec m il bl
neuronSpec =
  mconcat
    [ wikiLinkSpec,
      inlineTagSpec,
      highlightSpec,
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

inlineTagSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
inlineTagSpec =
  mempty
    { CM.syntaxInlineParsers = [pInlineTag]
    }
  where
    pInlineTag ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pInlineTag = P.try $ do
      _ <- symbol '#'
      tag <- CM.untokenize <$> inlineTagP
      case makeZTagURI tag of
        Nothing ->
          fail "Not an inline tag"
        Just (URI.render -> url) ->
          pure $! cmAutoLink url
    makeZTagURI :: Text -> Maybe URI
    makeZTagURI s = do
      tag <- mkPathPiece "tag"
      path <- traverse mkPathPiece $ T.splitOn "/" s
      pure $ URI (Just [scheme|z|]) (Left False) (Just (False, tag :| path)) [] Nothing

-- | Create a commonmark link element
cmAutoLink :: CM.IsInline a => Text -> a
cmAutoLink url =
  CM.link url title $ CM.str url
  where
    title = ""

wikiLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
wikiLinkSpec =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pLink = P.try $ do
      cmAutoLink
        <$> P.choice
          [ -- Folgezettel link: [[[...]]]
            P.try (wikiLinkP 3),
            -- Cf link: [[...]]
            P.try (wikiLinkP 2)
          ]
    wikiLinkP :: Monad m => Int -> P.ParsecT [CM.Tok] s m Text
    wikiLinkP n = do
      void $ M.count n $ symbol '['
      s <- fmap CM.untokenize $ some $ noneOfToks [Symbol ']', LineEnd]
      -- Parse as URI, add connection type, and then render back. If parse fails, we
      -- just ignore this inline.
      case makeZLinkURI s of
        Just uri -> do
          void $ M.count n $ symbol ']'
          pure $
            URI.render $ case n of
              3 ->
                -- [[[..]]] adds type=branch in URI
                uri {uriQuery = uriQuery uri <> [QueryParam [queryKey|type|] [queryValue|branch|]]}
              _ -> uri
        Nothing ->
          fail "Not a neuron URI; ignoring"
    -- Convert wiki-link to z: URI
    makeZLinkURI :: Text -> Maybe URI
    makeZLinkURI s =
      case toString s of
        ('z' : ':' : _) ->
          mkURI s
        _ -> do
          -- Treat it as plain ID
          path <- mkPathPiece s
          pure $ URI (Just [scheme|z|]) (Left True) (Just (False, path :| [])) [] Nothing

inlineTagP :: Monad m => P.ParsecT [CM.Tok] s m [CM.Tok]
inlineTagP =
  some (noneOfToks $ [Spaces, UnicodeSpace, LineEnd] <> fmap Symbol punctuation)
  where
    punctuation = "[];:,.?!"

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