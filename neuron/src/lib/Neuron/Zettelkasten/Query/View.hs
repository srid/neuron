{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: This module currently uses Pandoc AST to "render" stuff, but soonish it
-- will all be replaced by reflex-dom. See
-- https://github.com/srid/neuron/issues/170
module Neuron.Zettelkasten.Query.View
  ( buildQueryView,
    zettelUrl,
    tagUrl,
  )
where

import Control.Monad.Except
import Data.Default
import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import Data.Some
import Data.TagTree (Tag (..), TagNode (..), TagPattern (..), constructTag, foldTagTree, tagMatchAny, tagTree)
import qualified Data.Text as T
import Data.Tree
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.Pandoc.Builder as B

-- | Build the Pandoc AST for query results
buildQueryView :: MonadError QueryResultError m => DSum Query Identity -> m (Either B.Inline B.Block)
buildQueryView = \case
  Query_ZettelByID zid (fromMaybe def -> conn) :=> Identity mres ->
    case mres of
      Nothing -> throwError $ QueryResultError_NoSuchZettel zid
      Just target -> do
        pure $ Left $ buildZettelLink (Just conn) Nothing target
  q@(Query_ZettelsByTag pats (fromMaybe def -> conn) view) :=> Identity res ->
    pure $ Right $
      B.Div
        B.nullAttr
        [ buildQueryName $ Some q,
          case zettelsViewGroupByTag view of
            False ->
              buildZettelsLinks (Just conn) (zettelsViewLinkView view) res
            True ->
              B.Div B.nullAttr
                $ flip fmap (Map.toList $ groupZettelsByTagsMatching pats res)
                $ \(tag, zettelGrp) ->
                  B.Div
                    B.nullAttr
                    [ B.Plain $ pure $
                        B.Span
                          (mkAttr "ui basic pointing below grey label" mempty)
                          [ fontAwesomeIcon "fas fa-tag",
                            B.Str (unTag tag)
                          ],
                      buildZettelsLinks (Just conn) (zettelsViewLinkView view) zettelGrp
                    ]
        ]
  q@(Query_Tags _) :=> Identity res ->
    pure $ Right $
      B.Div
        B.nullAttr
        [ buildQueryName $ Some q,
          renderTagTree $ foldTagTree $ tagTree res
        ]
  where
    fontAwesomeIcon name =
      B.Span (mkAttr name mempty) []
    -- TODO: Instead of doing this here, group the results in runQuery itself.
    groupZettelsByTagsMatching pats matches =
      fmap sortZettelsReverseChronological $ Map.fromListWith (<>) $ flip concatMap matches $ \z ->
        flip concatMap (zettelTags z) $ \t -> [(t, [z]) | tagMatchAny pats t]
    mkAttr :: Text -> [(Text, Text)] -> B.Attr
    mkAttr cls kvs =
      ("", words cls, kvs)
    -- TODO: This should be consolidated with renderZettelLink in Zettel/View.hs
    -- (see module comment in this file)
    buildZettelLink :: Maybe Connection -> Maybe LinkView -> Zettel -> B.Inline
    buildZettelLink conn (fromMaybe def -> LinkView {..}) Zettel {..} =
      let connClass = bool "cf" "folgezettel" $ conn == Just Folgezettel
          linkTooltip =
            if null zettelTags
              then Nothing
              else Just $ "Tags: " <> T.intercalate "; " (unTag <$> zettelTags)
       in B.Span
            (mkAttr ("zettel-link-container " <> connClass) mempty)
            $ catMaybes
              [ if linkViewShowDate
                  then case zettelDay of
                    Just day ->
                      Just $ B.Span (mkAttr "extra monoFont" mempty) $ pure $ B.Str $ show day
                    Nothing -> Nothing
                  else Nothing,
                Just $ B.Span (mkAttr "zettel-link" $ semanticUITooltipAttrs linkTooltip) $ pure $
                  B.Link mempty [B.Str zettelTitle] (zettelUrl zettelID, "")
              ]
    buildZettelsLinks mconn view zs =
      B.BulletList $
        zs <&> \z ->
          [B.Plain $ pure $ buildZettelLink mconn (Just view) z]
    semanticUITooltipAttrs :: Maybe Text -> [(Text, Text)]
    semanticUITooltipAttrs = maybe [] $ \s ->
      [ ("data-tooltip", s),
        ("data-inverted", ""),
        ("data-position", "right center")
      ]
    buildQueryName :: Some Query -> B.Block
    buildQueryName someQ =
      B.Div (mkAttr "ui horizontal divider" [("title", "Neuron Query")]) $
        case someQ of
          Some (Query_ZettelByID _ _) ->
            []
          Some (Query_ZettelsByTag [] _mconn _mview) ->
            [B.Plain [B.Str "All zettels"]]
          Some (Query_ZettelsByTag (fmap unTagPattern -> pats) _mconn _mview) ->
            let qs = toText $ intercalate ", " pats
                desc = toText $ "Zettels tagged '" <> qs <> "'"
             in [ B.Plain $ pure $
                    B.Span
                      (mkAttr "ui basic pointing below black label" [("title", desc)])
                      [fontAwesomeIcon "fas fa-tags", B.Str qs]
                ]
          Some (Query_Tags []) ->
            [B.Plain $ pure $ B.Str $ "All tags"]
          Some (Query_Tags (fmap unTagPattern -> pats)) -> do
            let qs = toText $ intercalate ", " pats
            [B.Plain $ pure $ B.Str $ "Tags matching '" <> qs <> "'"]

-- TODO: not using Rib for ghcjs, but factorize this
zettelUrl :: ZettelID -> Text
zettelUrl zid =
  zettelIDText zid <> ".html"

tagUrl :: Tag -> Text
tagUrl (Tag s) =
  "search.html?tag=" <> s

-- |  Render a tag tree along with the count of zettels tagged with it
renderTagTree :: Forest (NonEmpty TagNode, Natural) -> B.Block
renderTagTree t =
  B.Div (mkAttr "tag-tree" mempty) $
    renderForest mempty t
  where
    renderForest :: [TagNode] -> Forest (NonEmpty TagNode, Natural) -> [B.Block]
    renderForest ancestors forest =
      pure $ B.BulletList $
        forest <&> \tree ->
          renderTree ancestors tree
    renderTree :: [TagNode] -> Tree (NonEmpty TagNode, Natural) -> [B.Block]
    renderTree ancestors (Node (tagNode, count) children) =
      mconcat
        [ [renderTag ancestors (tagNode, count)],
          renderForest (ancestors <> toList tagNode) $ toList children
        ]
    renderTag :: [TagNode] -> (NonEmpty TagNode, Natural) -> B.Block
    renderTag ancestors (tagNode, count) =
      let tag = constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
          tit = show count <> " zettels tagged"
       in B.Div (mkAttr "node" mempty) $ pure $ B.Plain $ pure $
            B.Link
              (mkAttr (bool "" "inactive" $ count == 0) mempty)
              [B.Str $ renderTagNode tagNode]
              (tagUrl tag, tit)
    renderTagNode :: NonEmpty TagNode -> Text
    renderTagNode = \case
      n :| (nonEmpty -> mrest) ->
        case mrest of
          Nothing ->
            unTagNode n
          Just rest ->
            unTagNode n <> "/" <> renderTagNode rest
    mkAttr cls kvs =
      ("", words cls, kvs)
