{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Markdown where

import qualified Commonmark as CM
import qualified Commonmark.Blocks as CM
-- import qualified Commonmark.Html as CM
import qualified Commonmark.Pandoc as CP
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens (TokType (..))
import Control.Monad.Combinators (manyTill)
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
--
-- TODO: Avoid error here
parseMarkdown :: (HasCallStack, YAML.FromYAML meta) => FilePath -> Text -> Either Text (meta, Pandoc)
parseMarkdown fn (partitionMarkdown fn -> (metaVal, s)) =
  case commonmarkPandocWith neuronSpec "markdown" s of
    Left e ->
      Left $ show e
    Right v ->
      Right (meta, Pandoc mempty $ B.toList (CP.unCm v))
  where
    meta =
      either (error . show) (maybe (error "No yaml val") head . nonEmpty)
        $ YAML.decode
        $ encodeUtf8 metaVal
    -- Like commonmarkWith, but parses directly into the Pandoc AST.
    commonmarkPandocWith ::
      CM.SyntaxSpec (Either P.ParseError) (CP.Cm () B.Inlines) (CP.Cm () B.Blocks) ->
      String ->
      Text ->
      Either P.ParseError (CP.Cm () B.Blocks)
    commonmarkPandocWith spec n =
      join . CM.commonmarkWith spec n

-- | Identify metadata block at the top, and split it from markdown body.
partitionMarkdown :: FilePath -> Text -> (Text, Text)
partitionMarkdown fn =
  either error id . parse splitP fn
  where
    separatorP :: Parser ()
    separatorP =
      void $ M.string "---" <* M.eol
    splitP :: Parser (Text, Text)
    splitP = do
      separatorP
      a <- toText <$> manyTill M.anySingle separatorP
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
      pure $! CM.link s (show x) $ CM.str s

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
