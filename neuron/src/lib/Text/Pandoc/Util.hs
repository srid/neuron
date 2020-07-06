{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Pandoc.Util
  ( getFirstParagraphText,
    getH1,
    plainify,
  )
where

import Relude
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

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
