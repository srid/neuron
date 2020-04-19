{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Control.Monad.Except
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import qualified Data.Map.Strict as Map
import Data.Some
import Lucid
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Markdown (MarkdownLink (..))
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Tag
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.URI as URI

-- | Query represents a way to query the Zettelkasten.
--
-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query r where
  Query_ZettelByID :: ZettelID -> Query Zettel
  Query_ZettelsByTag :: [TagPattern] -> Query [Zettel]
  Query_Tags :: [TagPattern] -> Query [Tag]

deriveGEq ''Query

deriveGShow ''Query

deriving instance Show (Query Zettel)

deriving instance Show (Query [Zettel])

deriving instance Show (Query [Tag])

deriving instance Eq (Query Zettel)

deriving instance Eq (Query [Zettel])

deriving instance Eq (Query [Tag])

instance ToHtml (Query [Zettel]) where
  toHtmlRaw = toHtml
  toHtml = \case
    Query_ZettelsByTag (fmap unTagPattern -> pats) ->
      div_ [class_ "ui horizontal divider", title_ "Zettel Query"] $ do
        if null pats
          then "All zettels"
          else
            let desc = "Zettels tagged '" <> show pats <> "'"
             in span_ [class_ "ui basic pointing below black label", title_ desc] $ toHtml $ show @Text pats

type QueryResults = [Zettel]

queryFromURI :: MonadError Text m => URI.URI -> m (Some Query)
queryFromURI uri = do
  mq <- queryFromMarkdownLink $ MarkdownLink {markdownLinkUri = uri, markdownLinkText = ""}
  case mq of
    Just q -> pure q
    Nothing -> throwError "Unsupported query URI"

-- NOTE: To support legacy links which rely on linkText. New short links shouldn't use this.
queryFromMarkdownLink :: MonadError Text m => MarkdownLink -> m (Maybe (Some Query))
queryFromMarkdownLink MarkdownLink {markdownLinkUri = uri, markdownLinkText = linkText} =
  case fmap URI.unRText (URI.uriScheme uri) of
    Just proto | proto `elem` ["z", "zcf"] -> do
      zid <- liftEither $ parseZettelID' linkText
      pure $ Just $ Some $ Query_ZettelByID zid
    Just proto
      | proto `elem` ["zquery", "zcfquery"]
          && uriHost uri == Right "search" ->
        pure $ Just $ Some $ Query_ZettelsByTag $ mkTagPattern <$> getParamValues "tag" uri
    _ -> pure $ do
      -- Initial support for the upcoming short links.
      guard $ URI.render uri == linkText
      zid <- rightToMaybe $ parseZettelID' linkText
      pure $ Some $ Query_ZettelByID zid
  where
    getParamValues k u =
      flip mapMaybe (URI.uriQuery u) $ \case
        URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
          if key == k
            then Just val
            else Nothing
        _ -> Nothing
    uriHost u =
      fmap (URI.unRText . URI.authHost) (URI.uriAuthority u)

-- | Run the given query and return the results.
runQuery :: ZettelStore -> Query r -> r
runQuery store = \case
  Query_ZettelByID zid ->
    lookupStore zid store
  Query_ZettelsByTag pats ->
    foldMap (queryResults pats) (Map.elems store)
  Query_Tags _pats ->
    -- TODO:
    []

matchQuery :: Zettel -> TagPattern -> Bool
matchQuery Zettel {..} pat =
  any (tagMatch pat) zettelTags

matchQueries :: Zettel -> [TagPattern] -> Bool
matchQueries zettel pats = and $ matchQuery zettel <$> pats

queryResults :: [TagPattern] -> Zettel -> [Zettel]
queryResults pats zettel
  | matchQueries zettel pats = [zettel]
  | otherwise = mempty
