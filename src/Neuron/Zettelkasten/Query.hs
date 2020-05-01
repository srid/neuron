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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Control.Monad.Except
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import qualified Data.Map.Strict as Map
import Data.Some
import Data.TagTree (Tag, TagPattern (..), mkTagPattern, tagMatch, tagMatchAny, tagTree)
import Data.Tree (Tree (..))
import Lucid
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Connection
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Theme
import Neuron.Zettelkasten.Zettel
import Relude
import System.FilePath
import Text.MMark.MarkdownLink (MarkdownLink (..))
import qualified Text.URI as URI

-- | Query represents a way to query the Zettelkasten.
--
-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query r where
  Query_ZettelByID :: ZettelID -> Maybe Connection -> Query (Maybe Zettel)
  Query_ZettelsByTag :: [TagPattern] -> Maybe Connection -> Maybe ZettelsView -> Query [Zettel]
  Query_Tags :: [TagPattern] -> Query (Map Tag Natural)

instance ToHtml (Some Query) where
  toHtmlRaw = toHtml
  toHtml q =
    div_ [class_ "ui horizontal divider", title_ "Neuron Query"] $ do
      case q of
        Some (Query_ZettelByID _ _) ->
          mempty
        Some (Query_ZettelsByTag [] _mconn _mview) ->
          "All zettels"
        Some (Query_ZettelsByTag (fmap unTagPattern -> pats) _mconn _mview) -> do
          let qs = intercalate ", " pats
              desc = toText $ "Zettels tagged '" <> qs <> "'"
           in span_ [class_ "ui basic pointing below black label", title_ desc] $ do
                i_ [class_ "tags icon"] mempty
                toHtml qs
        Some (Query_Tags []) ->
          "All tags"
        Some (Query_Tags (fmap unTagPattern -> pats)) -> do
          let qs = intercalate ", " pats
          toHtml $ "Tags matching '" <> qs <> "'"

type QueryResults = [Zettel]

queryFromURI :: MonadError QueryParseError m => URI.URI -> m (Some Query)
queryFromURI uri = do
  mq <- queryFromMarkdownLink $ MarkdownLink {markdownLinkUri = uri, markdownLinkText = ""}
  case mq of
    Just q -> pure q
    Nothing -> throwError $ QueryParseError_Unsupported uri

-- NOTE: To support legacy links which rely on linkText. New short links shouldn't use this.
queryFromMarkdownLink :: MonadError QueryParseError m => MarkdownLink -> m (Maybe (Some Query))
queryFromMarkdownLink ml@MarkdownLink {markdownLinkUri = uri, markdownLinkText = linkText} =
  case fmap URI.unRText (URI.uriScheme uri) of
    Just proto | proto `elem` ["z", "zcf"] -> do
      zid <- liftEither $ first (QueryParseError_InvalidID uri) $ parseZettelID' linkText
      pure $ Just $ Some $ Query_ZettelByID zid (connectionFromMarkdownLink ml)
    Just proto | proto `elem` ["zquery", "zcfquery"] ->
      case uriHost uri of
        Right "search" -> do
          mview <- liftEither $ first (QueryParseError_BadView uri) $ zettelsViewFromURI uri
          pure $ Just $ Some $
            Query_ZettelsByTag
              (mkTagPattern <$> getParamValues "tag" uri)
              (connectionFromMarkdownLink ml)
              mview
        Right "tags" ->
          pure $ Just $ Some $ Query_Tags $ mkTagPattern <$> getParamValues "filter" uri
        _ ->
          throwError $ QueryParseError_UnsupportedHost uri
    _ -> pure $ do
      -- Initial support for the upcoming short links.
      -- First, we expect that this is inside <..> (so same link text as link)
      guard $ URI.render uri == linkText
      -- Then, non-relevant parts of the URI should be empty
      guard
        `mapM_` [ URI.uriScheme uri == Nothing,
                  URI.uriAuthority uri == Left False,
                  URI.uriFragment uri == Nothing
                ]
      fmap snd (URI.uriPath uri) >>= \case
        (URI.unRText -> path) :| [] -> do
          zid <- rightToMaybe $ parseZettelID' path
          pure $ Some $ Query_ZettelByID zid (connectionFromMarkdownLink ml)
        _ ->
          -- Multiple path elements, not supported
          Nothing
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
runQuery :: [Zettel] -> Query r -> r
runQuery zs = \case
  Query_ZettelByID zid _ ->
    find ((== zid) . zettelID) zs
  Query_ZettelsByTag pats _mconn _mview ->
    sortZettelsReverseChronological $ flip filter zs $ \Zettel {..} ->
      and $ flip fmap pats $ \pat ->
        any (tagMatch pat) zettelTags
  Query_Tags [] ->
    allTags
  Query_Tags pats ->
    Map.filterWithKey (const . tagMatchAny pats) allTags
  where
    allTags :: Map.Map Tag Natural
    allTags =
      Map.fromListWith (+) $
        concatMap (\Zettel {..} -> (,1) <$> zettelTags) zs

queryResultJson :: forall r. (ToJSON (Query r)) => FilePath -> Query r -> r -> Value
queryResultJson notesDir q r =
  toJSON $
    object
      [ "query" .= toJSON q,
        "result" .= resultJson
      ]
  where
    resultJson :: Value
    resultJson = case q of
      Query_ZettelByID _ _mconn ->
        toJSON $ zettelJsonFull <$> r
      Query_ZettelsByTag _ _mconn _mview ->
        toJSON $ zettelJsonFull <$> r
      Query_Tags _ ->
        toJSON $ treeToJson <$> tagTree r
    zettelJsonFull z@Zettel {..} =
      object $
        [ "path" .= (notesDir </> zettelIDSourceFileName zettelID)
        ]
          <> zettelJson z
    treeToJson (Node (tag, count) children) =
      object
        [ "name" .= tag,
          "count" .= count,
          "children" .= fmap treeToJson children
        ]

deriveJSONGADT ''Query

deriveGEq ''Query

deriveGShow ''Query

deriving instance Show (Query (Maybe Zettel))

deriving instance Show (Query [Zettel])

deriving instance Show (Query (Map Tag Natural))

deriving instance Eq (Query (Maybe Zettel))

deriving instance Eq (Query [Zettel])

deriving instance Eq (Query (Map Tag Natural))
