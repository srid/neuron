{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Org where

import Data.TagTree
import Neuron.Zettelkasten.Zettel.Meta
import Neuron.Zettelkasten.Zettel.ParseError
import Relude
import Text.Pandoc (def, runPure)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition hiding (Meta (..))
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Util
import Prelude (lookup)

parseOrg ::
  FilePath ->
  Text ->
  Either ZettelParseError (Maybe Meta, Pandoc)
parseOrg _ s =
  case runPure (readOrg def s) of
    Left e -> Left $ ZettelParseError_InvalidOrg (show e)
    Right (Pandoc _ body) ->
      Right (extractMetadata body, Pandoc mempty body)

extractMetadata :: [B.Block] -> Maybe Meta
extractMetadata body = do
  ((_, _, properties), _) <- getH1 body
  date <- parseZettelDate <$> lookup "date" properties
  -- title is now deprecated
  let title = Nothing
      tags = fmap Tag . words <$> lookup "tags" properties
  pure $ Meta {..}
