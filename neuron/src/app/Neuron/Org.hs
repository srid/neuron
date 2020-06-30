{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Neuron.Org where

import Neuron.Zettelkasten.Zettel.Meta
import Neuron.Zettelkasten.Zettel.ParseError
import Relude hiding (show)
-- import Relude.Debug
import Text.Pandoc (def, runPure)
import qualified Text.Pandoc.Definition as Pandoc
import Text.Pandoc.Definition hiding (Meta (..))
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Writers.Native (writeNative)

parseOrg ::
  FilePath ->
  Text ->
  Either ZettelParseError (Maybe Meta, Pandoc)
parseOrg fn s = do
  traceShowId case runPure (readOrg def s) of
    Left e -> Left (ZettelParseError_InvalidOrg . toText $ show e)
    Right (Pandoc meta doc)
      | isNullMeta meta -> Right (Nothing, Pandoc nullMeta doc)
      | otherwise -> Right (Just $ pandocToNeuronMeta meta, Pandoc nullMeta doc)

pandocToNeuronMeta :: Pandoc.Meta -> Meta
pandocToNeuronMeta meta = traceShow meta Meta {..}
  where
    title = Nothing
    tags = case Pandoc.lookupMeta "tags" meta of
      Just tagsMeta -> Nothing
      Nothing -> Nothing
    date = do
      Pandoc.MetaInlines [Pandoc.Str dateText] <- Pandoc.lookupMeta "date" meta
      parseZettelDate dateText
