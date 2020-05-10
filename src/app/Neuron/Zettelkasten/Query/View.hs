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

module Neuron.Zettelkasten.Query.View
  ( renderZettelLink,
    buildQueryView,
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
import Lucid
import Lucid.Base (makeAttribute)
import Neuron.Web.Route (Route (..), routeUrlRelWithQuery)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib
import qualified Text.Pandoc.Builder as B
import Text.URI.QQ (queryKey)

-- | Build the Pandoc AST for query results
buildQueryView :: MonadError QueryResultError m => DSum Query Identity -> m (Either B.Inline B.Block)
buildQueryView = \case
  Query_ZettelByID zid _mconn :=> Identity mres ->
    case mres of
      Nothing -> throwError $ QueryResultError_NoSuchZettel zid
      Just target -> do
        pure $ Left $ buildZettelLink Nothing target
  q@(Query_ZettelsByTag pats _mconn view) :=> Identity res ->
    pure $ Right $
      B.Div
        B.nullAttr
        [ buildQueryName $ Some q,
          case zettelsViewGroupByTag view of
            False ->
              buildZettelsLinks (zettelsViewLinkView view) res
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
                      buildZettelsLinks (zettelsViewLinkView view) zettelGrp
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
    buildZettelLink (fromMaybe def -> LinkView {..}) Zettel {..} =
      -- TODO: not using Rib for ghcjs, but factorize this
      let zurl = "/" <> zettelIDText zettelID <> ".html"
          linkTooltip =
            if null zettelTags
              then Nothing
              else Just $ "Tags: " <> T.intercalate "; " (unTag <$> zettelTags)
       in B.Span
            (mkAttr "zettel-link-container" mempty)
            $ catMaybes
              [ if linkViewShowDate
                  then case zettelDay of
                    Just day ->
                      Just $ B.Span (mkAttr "extra" mempty) $ pure $ B.Str $ show day
                    Nothing -> Nothing
                  else Nothing,
                Just $ B.Span (mkAttr "zettel-link" $ semanticUITooltipAttrs linkTooltip) $ pure $
                  B.Link mempty [B.Str zettelTitle] (zurl, "")
              ]
    buildZettelsLinks view zs =
      B.BulletList $
        zs <&> \z ->
          [B.Plain $ pure $ buildZettelLink (Just view) z]
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

-- | Render a link to an individual zettel.
-- TODO: Remove and consolidate with pandoc renderer
renderZettelLink :: forall m. Monad m => Maybe LinkView -> Zettel -> HtmlT m ()
renderZettelLink (fromMaybe def -> LinkView {..}) Zettel {..} = do
  let zurl = Rib.routeUrlRel $ Route_Zettel zettelID
      mextra =
        if linkViewShowDate
          then case zettelDay of
            Just day ->
              Just $ toHtml $ show @Text day
            Nothing ->
              Nothing
          else Nothing
  span_ [class_ "zettel-link-container"] $ do
    forM_ mextra $ \extra ->
      span_ [class_ "extra"] extra
    let linkTooltip =
          if null zettelTags
            then Nothing
            else Just $ "Tags: " <> T.intercalate "; " (unTag <$> zettelTags)
    span_ ([class_ "zettel-link"] <> withTooltip linkTooltip) $ do
      a_ [href_ zurl] $ toHtml zettelTitle
  where
    withTooltip :: Maybe Text -> [Attribute]
    withTooltip = \case
      Nothing -> []
      Just s ->
        [ makeAttribute "data-tooltip" s,
          makeAttribute "data-inverted" "",
          makeAttribute "data-position" "right center"
        ]

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
      let Tag tag = constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
          url = routeUrlRelWithQuery Route_Search [queryKey|tag|] tag
          tit = show count <> " zettels tagged"
       in B.Div (mkAttr "node" mempty) $ pure $ B.Plain $ pure $
            B.Link
              (mkAttr (bool "" "inactive" $ count == 0) mempty)
              [B.Str $ renderTagNode tagNode]
              (url, tit)
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
