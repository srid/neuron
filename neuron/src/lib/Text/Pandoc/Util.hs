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
    setTitleH1,
    ensureTitleH1,
    plainify,
    PandocLink (..),
    mkPandocAutoLink,
    isAutoLink,
    getLinks,
  )
where

import Reflex.Dom.Pandoc.Util (plainify)
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

-- | Ensure that a title H1 is present in the Pandoc doc.
--
-- A "title H1" is a H1 that appears as the very first block element of the AST.
-- If this element is absent, then create it using the given text inline.
--
-- Either way, apply the given `id` attribute to the title H1 (so as to
-- distinguish it from other headings). And return the title plainify'ied.
ensureTitleH1 :: Text -> Text -> Pandoc -> (Text, Pandoc)
ensureTitleH1 idAttr defaultTitle = \case
  (Pandoc meta (B.Header 1 (_id, classes, kw) inlines : rest)) ->
    -- Add `idAttr` to existing H1
    let h1 = B.Header 1 (idAttr, classes, kw) inlines
        tit = plainify inlines
     in (tit,) $ Pandoc meta (h1 : rest)
  doc ->
    (defaultTitle,) $ setTitleH1 idAttr defaultTitle doc

setTitleH1 :: Text -> Text -> Pandoc -> Pandoc
setTitleH1 idAttr s (Pandoc meta rest) =
  let h1 = B.Header 1 (idAttr, mempty, mempty) [B.Str s]
   in Pandoc meta (h1 : rest)
