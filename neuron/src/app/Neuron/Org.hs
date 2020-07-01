{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Org
  ( orgReader,
  )
where

import Control.Monad.Writer
import Data.Some
import Data.TagTree
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Parser (queryFromURILink)
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.Zettel.Meta
import Neuron.Zettelkasten.Zettel.ParseError
import Reflex.Dom.Pandoc.URILink (queryURILinks)
import Relude
import Text.Pandoc (ReaderOptions (..), def, runPure)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition hiding (Meta (..))
import Text.Pandoc.Extensions
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Util
import Prelude (lookup)

orgReader :: ZettelReader
orgReader =
  ZettelReader
    { readZettel = parseOrg,
      extractQueries = extractOrgQueries
    }

orgOptions :: ReaderOptions
orgOptions =
  def
    { readerExtensions = getDefaultExtensions "org"
    }

parseOrg ::
  FilePath ->
  Text ->
  Either ZettelParseError (Maybe Meta, Pandoc)
parseOrg _ s =
  case runPure (readOrg orgOptions s) of
    Left e -> Left $ ZettelParseError_InvalidOrg (show e)
    Right (Pandoc _ body) ->
      Right (extractMetadata body, Pandoc mempty body)

extractMetadata :: [B.Block] -> Maybe Meta
extractMetadata body = traceShow body $ do
  ((_, _, properties), _) <- getH1 body
  date <- parseZettelDate <$> lookup "date" properties
  -- title is now deprecated
  let title = Nothing
      tags = fmap Tag . words <$> lookup "tags" properties
  pure $ Meta {..}

extractOrgQueries :: MonadWriter [QueryParseError] m => Pandoc -> m [Some ZettelQuery]
extractOrgQueries doc =
  fmap catMaybes $ forM (queryURILinks doc) $ \ul ->
    case queryFromURILink ul of
      Left e -> do
        tell [e]
        pure Nothing
      Right v ->
        pure v
