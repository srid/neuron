{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Parser where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Some (Some (..))
import Data.TagTree (Tag, unTagPattern)
import qualified Data.Text as T
import Neuron.Reader.Type (ZettelFormat, ZettelReader)
import Neuron.Zettelkasten.ID (ZettelID (zettelIDRaw))
import Neuron.Zettelkasten.Query.Parser (parseQueryLink)
import Neuron.Zettelkasten.Zettel
  ( ZettelC,
    ZettelQuery (..),
    ZettelT (Zettel),
  )
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
import Relude
import Text.Pandoc.Definition (Block (Plain), Inline (Code, Str), nullAttr)
import qualified Text.Pandoc.Util as P

parseZettel ::
  ZettelFormat ->
  ZettelReader ->
  FilePath ->
  ZettelID ->
  Text ->
  ZettelC
parseZettel format zreader fn zid s = do
  case zreader fn s of
    Left parseErr ->
      Left $ Zettel zid format fn "Unknown" False [] Nothing False [] (Just parseErr) s
    Right (meta, doc) ->
      let -- Determine zettel title
          (title, titleInBody) = case Meta.title =<< meta of
            Just tit -> (tit, False)
            Nothing -> fromMaybe (zettelIDRaw zid, False) $ do
              ((,True) . P.plainify . snd <$> P.getH1 doc)
          -- Accumulate queries
          queries =
            mapMaybe (uncurry parseQueryLinkWithContext) $
              Map.toList $ P.getLinksWithContext doc
          -- Determine zettel tags
          metaTags = fromMaybe [] $ Meta.tags =<< meta
          queryTags = (getInlineTag . fst) `mapMaybe` queries
          tags = nub $ metaTags <> queryTags
          -- Determine other metadata
          date = Meta.date =<< meta
          unlisted = fromMaybe False $ Meta.unlisted =<< meta
       in Right $ Zettel zid format fn title titleInBody tags date unlisted queries Nothing doc
  where
    getInlineTag :: Some ZettelQuery -> Maybe Tag
    getInlineTag = \case
      Some (ZettelQuery_TagZettel tag) -> Just tag
      _ -> Nothing
    parseQueryLinkWithContext uri ctx = do
      case parseQueryLink uri of
        Nothing -> Nothing
        Just someQ ->
          Just (someQ, convertCtx ctx someQ)
    convertCtx :: [Block] -> Some ZettelQuery -> [Block]
    convertCtx ctx = \case
      Some (ZettelQuery_ZettelByID _ _) ->
        ctx
      Some (ZettelQuery_ZettelsByTag (nonEmpty -> Nothing) _ _) ->
        one $ Plain [Str "All zettels query"]
      Some (ZettelQuery_ZettelsByTag (nonEmpty -> Just pats) _ _) ->
        let tagsStr = T.intercalate ", " $ toText . unTagPattern <$> toList pats
         in one $ Plain [Str "Linking by tag: ", Code nullAttr tagsStr]
      _ ->
        mempty

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  [((ZettelFormat, ZettelReader), [(ZettelID, FilePath, Text)])] ->
  [ZettelC]
parseZettels filesPerFormat =
  flip concatMap filesPerFormat $ \((format, zreader), files) ->
    flip fmap files $ \(zid, path, s) ->
      parseZettel format zreader path zid s
