{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Parser where

import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.Some
import qualified Data.Text as T
import qualified Neuron.Markdown as MD
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Parser (queryFromURILink)
import Neuron.Zettelkasten.Zettel
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
import Reflex.Dom.Pandoc.URILink (queryURILinks)
import Relude
import System.FilePath (takeExtension)
import Text.Pandoc.Definition (Pandoc)

-- | Parse a markdown-formatted zettel
--
-- In future this will support other formats supported by Pandoc.
parseZettel ::
  ZettelReader ->
  ZettelID ->
  Text ->
  ZettelC
parseZettel zreader zid s = do
  case zreader s of
    Left parseErr ->
      Left $ Zettel zid "Unknown" False [] Nothing [] parseErr s
    Right (meta, doc) ->
      let (title, titleInBody) = case Meta.title =<< meta of
            Just tit -> (tit, False)
            Nothing -> fromMaybe ("Untitled", False) $ do
              ((,True) . MD.plainify <$> MD.getH1 doc)
                <|> ((,False) . takeInitial . MD.plainify <$> MD.getFirstParagraphText doc)
          tags = fromMaybe [] $ Meta.tags =<< meta
          day = case zid of
            -- We ignore the "data" meta field on legacy Date IDs, which encode the
            -- creation date in the ID.
            ZettelDateID v _ -> Just v
            ZettelCustomID _ -> Meta.date =<< meta
          (queries, errors) = runWriter $ extractQueries doc
       in Right $ Zettel zid title titleInBody tags day queries errors doc
  where
    -- Extract all (valid) queries from the Pandoc document
    extractQueries :: MonadWriter [QueryParseError] m => Pandoc -> m [Some ZettelQuery]
    extractQueries doc =
      fmap catMaybes $ forM (queryURILinks doc) $ \ul ->
        case queryFromURILink ul of
          Left e -> do
            tell [e]
            pure Nothing
          Right v ->
            pure v
    takeInitial =
      (<> " ...") . T.take 18

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  Map.Map Text (FilePath -> ZettelReader) ->
  [(FilePath, Text)] ->
  [ZettelC]
parseZettels readers fs =
  flip mapMaybe fs $ \(path, s) -> do
    -- TODO either use fromJust since this is supposed to be unreachable
    --      or report unsupported extension
    zreader <- Map.lookup (toText $ takeExtension path) readers
    zid <- getZettelID path
    pure $ parseZettel (zreader path) zid s
