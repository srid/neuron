{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Parser where

import Control.Monad.Writer
import Data.List (nub)
import Data.Some (Some (..))
import Data.TagTree (Tag)
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
          queries = mapMaybe (parseQueryLink . P._pandocLink_uri) $ P.getLinks doc
          -- Determine zettel tags
          metaTags = fromMaybe [] $ Meta.tags =<< meta
          queryTags = getInlineTag `mapMaybe` queries
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

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  [((ZettelFormat, ZettelReader), [(ZettelID, FilePath, Text)])] ->
  [ZettelC]
parseZettels filesPerFormat =
  flip concatMap filesPerFormat $ \((format, zreader), files) ->
    flip fmap files $ \(zid, path, s) ->
      parseZettel format zreader path zid s
