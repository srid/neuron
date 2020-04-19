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
import qualified Data.Set as Set
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

instance ToHtml (Some Query) where
  toHtmlRaw = toHtml
  toHtml q =
    div_ [class_ "ui horizontal divider", title_ "Neuron Query"] $ do
      case q of
        Some (Query_ZettelByID _) ->
          mempty
        Some (Query_ZettelsByTag []) ->
          "All zettels"
        Some (Query_ZettelsByTag (fmap unTagPattern -> pats)) -> do
          let qs = intercalate ", " pats
              desc = toText $ "Zettels tagged '" <> qs <> "'"
           in span_ [class_ "ui basic pointing below black label", title_ desc] $ toHtml qs
        Some (Query_Tags []) ->
          "All tags"
        Some (Query_Tags (fmap unTagPattern -> pats)) -> do
          let qs = intercalate ", " pats
          "Tags matching: "
          toHtml qs

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
    Just proto | proto `elem` ["zquery", "zcfquery"] ->
      case uriHost uri of
        Right "search" ->
          pure $ Just $ Some $ Query_ZettelsByTag $ mkTagPattern <$> getParamValues "tag" uri
        Right "tags" ->
          pure $ Just $ Some $ Query_Tags $ mkTagPattern <$> getParamValues "filter" uri
        _ ->
          throwError "Unsupported host in zquery"
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
    flip filter (Map.elems store) $ \Zettel {..} ->
      and $ flip fmap pats $ \pat ->
        any (tagMatch pat) zettelTags
  Query_Tags [] ->
    allTags
  Query_Tags pats ->
    filter (tagMatchAny pats) allTags
  where
    allTags = Set.toList $ Set.fromList $ foldMap zettelTags (Map.elems store)

deriveGEq ''Query

deriveGShow ''Query

deriving instance Show (Query Zettel)

deriving instance Show (Query [Zettel])

deriving instance Show (Query [Tag])

deriving instance Eq (Query Zettel)

deriving instance Eq (Query [Zettel])

deriving instance Eq (Query [Tag])
