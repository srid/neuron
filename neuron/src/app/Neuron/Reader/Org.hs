{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.Org where

import Data.TagTree
import Neuron.Zettelkasten.Zettel.Meta (Meta (..), parseZettelDate)
import Relude
import Text.Pandoc (def, runPure)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition hiding (Meta (..))
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Util (getH1)
import Prelude (lookup)

-- REVIEW Use parseOrg :: ZettelReader?

-- | Parse Org document, extracting the zettel metadata.
parseOrg ::
  FilePath ->
  Text ->
  Either Text (Maybe Meta, Pandoc)
parseOrg _ s =
  case runPure (readOrg def s) of
    Left e -> Left $ show e
    Right (Pandoc _ body) -> do
      meta <- extractMetadata body
      pure (meta, Pandoc mempty body)

-- | Extract metadata from the properties that are attached to the first headline
extractMetadata :: [B.Block] -> Either Text (Maybe Meta)
extractMetadata body
  | Just ((_, _, properties), _) <- getH1 body,
    not (null properties) = do
    date <- traverse parseZettelDate $ lookup "date" properties
    -- title is now deprecated
    let title = Nothing
        tags = fmap Tag . words <$> lookup "tags" properties
    pure $ Just Meta {..}
  | otherwise = pure Nothing
