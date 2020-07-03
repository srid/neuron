{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Org where

import Data.TagTree
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Neuron.Zettelkasten.Zettel.Meta (Meta (..), parseZettelDate)
import Neuron.Zettelkasten.Zettel.ParseError (ZettelParseError (..))
import Relude
import Text.Pandoc (def, runPure)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition hiding (Meta (..))
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Util (getH1)
import qualified Text.Pandoc.Walk as W
import Prelude (lookup)

parseOrg ::
  FilePath ->
  Text ->
  Either ZettelParseError (Maybe Meta, Pandoc)
parseOrg _ s =
  case runPure (readOrg def s) of
    Left e -> Left $ ZettelParseError_InvalidOrg (show e)
    Right (Pandoc _ body) ->
      Right (extractMetadata body, rewriteLinks $ Pandoc mempty body)

extractMetadata :: [B.Block] -> Maybe Meta
extractMetadata body = do
  ((_, _, properties), _) <- getH1 body
  date <- parseZettelDate <$> lookup "date" properties
  -- title is now deprecated
  let title = Nothing
      tags = fmap Tag . words <$> lookup "tags" properties
  pure $ Meta {..}

rewriteLinks :: Pandoc -> Pandoc
rewriteLinks = W.walk \case
  -- REVIEW What should I do with the title?
  link@(Link attr _ (src, title)) ->
    let processQuery (query, rest)
          | Text.null rest = Link attr [Str query] (query, title)
          | otherwise = link
     in either (const link) processQuery $ queryReader src
  inline -> inline

-- REVIEW Use Text.URI?
queryReader :: Text.Reader Text
queryReader src
  | auth == "z:/" = Right ("z:zettel/" <> rest, "")
  | otherwise = Left "No"
  where
    (auth, rest) = Text.splitAt 3 src
