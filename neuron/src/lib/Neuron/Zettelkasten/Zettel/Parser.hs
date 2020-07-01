{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Parser where

import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.Zettel.Format
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
import Relude
import Relude.Unsafe (fromJust)
import System.FilePath (takeExtension)
import Text.Pandoc.Util

-- | Parse a markdown-formatted zettel
--
-- In future this will support other formats supported by Pandoc.
parseZettel ::
  ZettelReader ->
  FilePath ->
  ZettelID ->
  Text ->
  ZettelC
parseZettel ZettelReader {..} fn zid s = do
  let fmt = fromJust $ extensionToZettelFormat $ toText $ takeExtension fn
  case readZettel fn s of
    Left parseErr ->
      Left $ Zettel zid fmt "Unknown" False [] Nothing [] parseErr s
    Right (meta, doc) ->
      let (title, titleInBody) = case Meta.title =<< meta of
            Just tit -> (tit, False)
            Nothing -> fromMaybe ("Untitled", False) $ do
              ((,True) . plainify . snd <$> getH1 doc)
                <|> ((,False) . takeInitial . plainify <$> getFirstParagraphText doc)
          tags = fromMaybe [] $ Meta.tags =<< meta
          day = case zid of
            -- We ignore the "data" meta field on legacy Date IDs, which encode the
            -- creation date in the ID.
            ZettelDateID v _ -> Just v
            ZettelCustomID _ -> Meta.date =<< meta
          (queries, errors) = runWriter $ extractQueries doc
       in Right $ Zettel zid fmt title titleInBody tags day queries errors doc
  where
    takeInitial =
      (<> " ...") . T.take 18

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  Map.Map Text ZettelReader ->
  [(FilePath, Text)] ->
  [ZettelC]
parseZettels readers fs =
  flip mapMaybe fs $ \(path, s) -> do
    -- TODO either use fromJust since this is supposed to be unreachable
    --      or report unsupported extension
    zreader <- Map.lookup (toText $ takeExtension path) readers
    zid <- getZettelID path
    pure $ parseZettel zreader path zid s
