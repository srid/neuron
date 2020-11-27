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
import Neuron.Zettelkasten.ID (ZettelID (unZettelID), Slug)
import Neuron.Zettelkasten.Query.Parser (parseQueryLink)
import Neuron.Zettelkasten.Zettel
  ( ZettelC,
    ZettelQuery (..),
    ZettelT (Zettel),
  )
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
import Relude
import Text.Pandoc.Definition (Block (Plain), Inline (Code, Str), Pandoc, nullAttr)
import qualified Text.Pandoc.Util as P

parseZettel ::
  ZettelFormat ->
  ZettelReader ->
  QueryExtractor ->
  FilePath ->
  ZettelID ->
  Text ->
  ZettelC
parseZettel format zreader queryExtractor fn zid s = do
  case zreader fn s of
    Left parseErr ->
      let slug = mkDefaultSlug $ unZettelID zid
       in Left $ Zettel zid slug format fn "Unknown" False [] Nothing False [] (Just parseErr) s
    Right (meta, doc) ->
      let -- Determine zettel title
          (title, titleInBody) = case Meta.title =<< meta of
            Just tit -> (tit, False)
            Nothing -> fromMaybe (unZettelID zid, False) $ do
              (,True) . P.plainify . snd <$> P.getH1 doc
          -- Accumulate queries
          queries = queryExtractor doc
          -- Determine zettel tags
          metaTags = fromMaybe [] $ Meta.tags =<< meta
          queryTags = (getInlineTag . fst) `mapMaybe` queries
          tags = nub $ metaTags <> queryTags
          -- Determine other metadata
          date = Meta.date =<< meta
          slug = fromMaybe (mkDefaultSlug $ unZettelID zid) $ Meta.slug =<< meta
          unlisted = Just True == (Meta.unlisted =<< meta)
       in Right $ Zettel zid slug format fn title titleInBody tags date unlisted queries Nothing doc
  where
    getInlineTag :: Some ZettelQuery -> Maybe Tag
    getInlineTag = \case
      Some (ZettelQuery_TagZettel tag) -> Just tag
      _ -> Nothing
    mkDefaultSlug :: Text -> Slug
    mkDefaultSlug =
      T.intercalate "_" . T.splitOn " "

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  QueryExtractor ->
  [((ZettelFormat, ZettelReader), [(ZettelID, FilePath, Text)])] ->
  [ZettelC]
parseZettels queryExtractor filesPerFormat =
  flip concatMap filesPerFormat $ \((format, zreader), files) ->
    flip fmap files $ \(zid, path, s) ->
      parseZettel format zreader queryExtractor path zid s

type QueryExtractor = Pandoc -> [(Some ZettelQuery, [Block])]

extractQueriesWithContext :: QueryExtractor
extractQueriesWithContext doc =
  mapMaybe (uncurry parseQueryLinkWithContext) $
    Map.toList $ P.getLinksWithContext doc
  where
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

extractQueriesWithoutContext :: QueryExtractor
extractQueriesWithoutContext doc =
  flip mapMaybe (P.getLinks doc) $ \(P._pandocLink_uri -> uri) -> do
    someQ <- parseQueryLink uri
    pure (someQ, mempty)
