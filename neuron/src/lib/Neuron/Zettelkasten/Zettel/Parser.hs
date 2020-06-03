{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Parser where

import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.Some
import Neuron.Markdown
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Parser (queryFromURILink)
import Neuron.Zettelkasten.Zettel
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
import Reflex.Class (filterLeft, filterRight)
import Reflex.Dom.Pandoc.URILink (queryURILinks)
import Relude
import Text.Pandoc.Definition (Pandoc)

-- | Parse a markdown-formatted zettel
--
-- In future this will support other formats supported by Pandoc.
parseZettel ::
  ZettelID ->
  Text ->
  Either ZettelParseError PandocZettel
parseZettel zid s = do
  (meta, doc) <- parseMarkdown (zettelIDSourceFileName zid) s
  let title = maybe "Missing title" Meta.title meta
      tags = fromMaybe [] $ Meta.tags =<< meta
      day = case zid of
        -- We ignore the "data" meta field on legacy Date IDs, which encode the
        -- creation date in the ID.
        ZettelDateID v _ -> Just v
        ZettelCustomID _ -> Meta.date =<< meta
      (queries, errors) = runWriter $ extractQueries doc
  pure $ PandocZettel (Zettel zid title tags day (queries, errors), doc)
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

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  [(FilePath, Text)] ->
  ( [PandocZettel],
    -- List of zettel files that cannot be parsed.
    Map ZettelID ZettelParseError
  )
parseZettels fs =
  let res = flip mapMaybe fs $ \(f, s) ->
        case getZettelID f of
          Nothing -> Nothing
          Just zid ->
            Just $ first (zid,) $ parseZettel zid s
      errors = filterLeft res
      zs = filterRight res
   in (zs, Map.fromList errors)
