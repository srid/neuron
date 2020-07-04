{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.Org where

import qualified Data.Map as Map
import Data.TagTree (Tag (Tag))
import Neuron.Reader.Type (ZettelReader)
import Neuron.Zettelkasten.Zettel.Meta (Meta (..), parseZettelDate)
import Relude
import Relude.Extra.Map (lookup)
import Text.Pandoc (def, runPure)
import Text.Pandoc.Definition hiding (Meta (..))
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Util (getH1)

-- | Parse Org document, extracting the zettel metadata.
parseOrg :: ZettelReader
parseOrg _ s = do
  doc <- first show $ runPure $ readOrg def s
  meta <- extractMetadata doc
  pure (meta, doc)

-- | Extract metadata from the properties that are attached to the first headline
extractMetadata :: Pandoc -> Either Text (Maybe Meta)
extractMetadata doc
  | Just ((_, _, Map.fromList -> properties), _) <- getH1 doc = do
    date <- traverse parseDate $ lookup "date" properties
    -- title is now deprecated
    let title = Nothing
        tags = fmap Tag . words <$> lookup "tags" properties
    pure $ Just Meta {..}
  | otherwise = pure Nothing
  where
    parseDate date = maybeToRight ("Invalid date format: " <> date) $ parseZettelDate date
