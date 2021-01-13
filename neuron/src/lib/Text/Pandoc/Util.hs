{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO Review and delete what's unused in this module
module Text.Pandoc.Util
  ( getFirstParagraphText,
    getH1,
    plainify,
    PandocLink (..),
    mkPandocAutoLink,
    isAutoLink,
    getLinks,
  )
where

import Relude
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import Text.URI (URI)

data PandocLink = PandocLink
  { -- | This is set to Nothing for autolinks
    _pandocLink_inner :: Maybe [B.Inline],
    _pandocLink_uri :: URI
  }
  deriving (Eq, Show, Ord)

mkPandocAutoLink :: URI -> PandocLink
mkPandocAutoLink = PandocLink Nothing

isAutoLink :: PandocLink -> Bool
isAutoLink PandocLink {..} =
  isNothing _pandocLink_inner

-- | Get all links that have a valid URI
-- TODO: Move to pandoc-link-context
getLinks :: W.Walkable B.Inline b => b -> [([(Text, Text)], Text)]
getLinks = W.query go
  where
    go :: B.Inline -> [([(Text, Text)], Text)]
    go = maybeToList . uriLinkFromInline
    uriLinkFromInline :: B.Inline -> Maybe ([(Text, Text)], Text)
    uriLinkFromInline inline = do
      B.Link (_, _, attrs) _inlines (url, title) <- pure inline
      pure (("title", title) : attrs, url)

getFirstParagraphText :: Pandoc -> Maybe [B.Inline]
getFirstParagraphText = listToMaybe . W.query go
  where
    go :: B.Block -> [[B.Inline]]
    go = \case
      B.Para inlines ->
        [inlines]
      _ ->
        []

getH1 :: Pandoc -> Maybe (B.Attr, [B.Inline])
getH1 = listToMaybe . W.query go
  where
    go :: B.Block -> [(B.Attr, [B.Inline])]
    go = \case
      B.Header 1 attr inlines ->
        [(attr, inlines)]
      _ ->
        []

-- | Convert Pandoc AST inlines to raw text.
plainify :: [B.Inline] -> Text
plainify = W.query $ \case
  B.Str x -> x
  B.Code _attr x -> x
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> " "
  B.RawInline _fmt s -> s
  B.Math _mathTyp s -> s
  -- Ignore the rest of AST nodes, as they are recursively defined in terms of
  -- `Inline` which `W.query` will traverse again.
  _ -> ""
