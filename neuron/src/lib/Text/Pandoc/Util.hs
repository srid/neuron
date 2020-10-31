{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Pandoc.Util
  ( getFirstParagraphText,
    getH1,
    plainify,
    PandocLink (..),
    mkPandocAutoLink,
    isAutoLink,
    getLinks,
    getLinksWithContext,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Relude
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import Text.URI (URI)
import qualified Text.URI as URI

data PandocLink = PandocLink
  { -- | This is set to Nothing for autolinks
    _pandocLink_inner :: Maybe [B.Inline],
    _pandocLink_uri :: URI
  }
  deriving (Eq, Show, Ord)

mkPandocAutoLink :: URI -> PandocLink
mkPandocAutoLink uri =
  PandocLink Nothing uri

isAutoLink :: PandocLink -> Bool
isAutoLink PandocLink {..} =
  isNothing _pandocLink_inner

-- | Get all links that have a valid URI
getLinks :: W.Walkable B.Inline b => b -> [PandocLink]
getLinks = W.query go
  where
    go :: B.Inline -> [PandocLink]
    go = maybeToList . uriLinkFromInline
    uriLinkFromInline :: B.Inline -> Maybe PandocLink
    uriLinkFromInline inline = do
      B.Link _attr inlines (url, _title) <- pure inline
      uri <- URI.mkURI url
      let inner = do
            guard $ inlines /= [B.Str url]
            pure inlines
      pure $ PandocLink inner uri

getLinksWithContext :: Pandoc -> Map URI [B.Block]
getLinksWithContext doc =
  fmap (reverse . nub) $ Map.fromListWith (<>) . fmap (second one) $ W.query go doc
  where
    go :: B.Block -> [(URI, B.Block)]
    go blk =
      fmap (,blk) $ case blk of
        B.Para is ->
          W.query linksFromInline is
        B.Plain is ->
          W.query linksFromInline is
        B.LineBlock is ->
          W.query linksFromInline is
        B.Header _ _ is ->
          W.query linksFromInline is
        B.DefinitionList xs ->
          -- Gather all filenames linked, and have them put (see above) in the
          -- same definition list block.
          concat $
            flip fmap xs $ \(is, bss) ->
              let def = W.query linksFromInline is
                  body = fmap (fmap (fmap fst . go)) bss
               in def <> concat (concat body)
        _ -> mempty

    linksFromInline :: B.Inline -> [URI]
    linksFromInline = fmap _pandocLink_uri . getLinks

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
