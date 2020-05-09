{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Markdown where

import qualified Commonmark as CM
import qualified Commonmark.Blocks as CM
-- import qualified Commonmark.Html as CM
import qualified Commonmark.Pandoc as CP
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens (TokType (..))
import Relude
import Text.MMark.MarkdownLink -- TODO: make it not mmark specific
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import qualified Text.Parsec as P
import qualified Text.URI as URI

parseMarkdown :: Text -> Either Text Pandoc
parseMarkdown s =
  bimap show (Pandoc meta . B.toList . CP.unCm) $
    commonmarkPandocWith
      neuronSpec
      "markdown"
      s
  where
    -- TODO: Parse yaml here
    meta = mempty
    commonmarkPandocWith ::
      CM.SyntaxSpec (Either P.ParseError) (CP.Cm () B.Inlines) (CP.Cm () B.Blocks) ->
      String ->
      Text ->
      Either P.ParseError (CP.Cm () B.Blocks)
    commonmarkPandocWith spec n =
      join . CM.commonmarkWith spec n

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

neuronSpec :: (Monad m, CM.IsBlock il bl, CM.IsInline il) => CM.SyntaxSpec m il bl
neuronSpec =
  mconcat
    [ angleBracketLinkSpec,
      CM.defaultSyntaxSpec {CM.syntaxBlockSpecs = defaultBlockSpecsSansRawHtml}
    ]

angleBracketLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
angleBracketLinkSpec =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink = P.try $ do
      void $ symbol '<'
      x <- some (noneOfToks [Symbol '>', Spaces, UnicodeSpace, LineEnd])
      let s = CM.untokenize x
      void $ symbol '>'
      pure $! CM.link ("/" <> s) (show x) $ CM.str s

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
    -- , rawHtmlSpec
    CM.attributeSpec
  ]
