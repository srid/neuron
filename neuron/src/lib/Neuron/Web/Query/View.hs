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

module Neuron.Web.Query.View
  ( renderQueryResultIfSuccessful,
    renderZettelLink,
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
import Neuron.Web.Widget
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding (count, tag)
import Relude

-- | Render the query results.
--
-- If the query result is unexpected, don't render anything and return
-- `QueryResultError` which the caller is expected to handle.
renderQueryResultIfSuccessful ::
  DomBuilder t m => DSum ZettelQuery Identity -> m (Maybe QueryResultError)
renderQueryResultIfSuccessful = \case
  ZettelQuery_ZettelByID zid (fromMaybe def -> conn) :=> Identity mres ->
    case mres of
      Nothing ->
        pure $ Just $ QueryResultError_NoSuchZettel zid
      Just target -> do
        renderZettelLink (Just conn) Nothing target
        pure Nothing
  q@(ZettelQuery_ZettelsByTag pats (fromMaybe def -> conn) view) :=> Identity res -> do
    el "section" $ do
      renderQuery $ Some q
      case zettelsViewGroupByTag view of
        False ->
          el "ul" $ forM_ res $ \z -> do
            el "li" $
              renderZettelLink (Just conn) (Just $ zettelsViewLinkView view) z
        True ->
          forM_ (Map.toList $ groupZettelsByTagsMatching pats res) $ \(tag, zettelGrp) -> do
            el "section" $ do
              elClass "span" "ui basic pointing below grey label" $ do
                elClass "span" "fas fa-tag" blank
                text $ unTag tag
              el "ul" $ forM_ zettelGrp $ \z ->
                el "li" $
                  renderZettelLink (Just conn) (Just $ zettelsViewLinkView view) z
    pure Nothing
  q@(ZettelQuery_Tags _) :=> Identity res -> do
    el "section" $ do
      renderQuery $ Some q
      renderTagTree $ foldTagTree $ tagTree res
    pure Nothing
  where
    -- TODO: Instead of doing this here, group the results in runQuery itself.
    groupZettelsByTagsMatching pats matches =
      fmap sortZettelsReverseChronological $ Map.fromListWith (<>) $ flip concatMap matches $ \z ->
        flip concatMap (zettelTags z) $ \t -> [(t, [z]) | tagMatchAny pats t]

renderQuery :: DomBuilder t m => Some ZettelQuery -> m ()
renderQuery someQ =
  elAttr "div" ("class" =: "ui horizontal divider" <> "title" =: "Neuron ZettelQuery") $ do
    case someQ of
      Some (ZettelQuery_ZettelByID _ _) ->
        blank
      Some (ZettelQuery_ZettelsByTag [] _mconn _mview) ->
        text "All zettels"
      Some (ZettelQuery_ZettelsByTag (fmap unTagPattern -> pats) _mconn _mview) -> do
        let qs = toText $ intercalate ", " pats
            desc = toText $ "Zettels tagged '" <> qs <> "'"
        elAttr "span" ("class" =: "ui basic pointing below black label" <> "title" =: desc) $ do
          elClass "span" "fas fa-tags" blank
          text qs
      Some (ZettelQuery_Tags []) ->
        text "All tags"
      Some (ZettelQuery_Tags (fmap unTagPattern -> pats)) -> do
        let qs = toText $ intercalate ", " pats
        text $ "Tags matching '" <> qs <> "'"

-- | Render a link to an individual zettel.
renderZettelLink :: DomBuilder t m => Maybe Connection -> Maybe LinkView -> Zettel -> m ()
renderZettelLink conn (fromMaybe def -> LinkView {..}) Zettel {..} = do
  let connClass = maybe "" show conn
      mextra =
        if linkViewShowDate
          then case zettelDay of
            Just day ->
              Just $ elTime day
            Nothing ->
              Nothing
          else Nothing
  elClass "span" ("zettel-link-container " <> connClass) $ do
    forM_ mextra $ \extra ->
      elClass "span" "extra monoFont" $ extra
    let linkTooltip =
          if null zettelTags
            then Nothing
            else Just $ "Tags: " <> T.intercalate "; " (unTag <$> zettelTags)
    elAttr "span" ("class" =: "zettel-link" <> withTooltip linkTooltip) $ do
      elAttr "a" ("href" =: (zettelUrl zettelID)) $ text zettelTitle
  where
    withTooltip :: Maybe Text -> Map Text Text
    withTooltip = \case
      Nothing -> mempty
      Just s ->
        ( "data-tooltip" =: s
            <> "data-inverted" =: ""
            <> "data-position" =: "right center"
        )

renderTagTree :: forall t m. DomBuilder t m => Forest (NonEmpty TagNode, Natural) -> m ()
renderTagTree t =
  divClass "tag-tree" $
    renderForest mempty t
  where
    renderForest :: [TagNode] -> Forest (NonEmpty TagNode, Natural) -> m ()
    renderForest ancestors forest =
      el "ul" $ do
        forM_ forest $ \tree ->
          el "li" $ renderTree ancestors tree
    renderTree :: [TagNode] -> Tree (NonEmpty TagNode, Natural) -> m ()
    renderTree ancestors (Node (tagNode, count) children) = do
      renderTag ancestors (tagNode, count)
      renderForest (ancestors <> toList tagNode) $ toList children
    renderTag :: [TagNode] -> (NonEmpty TagNode, Natural) -> m ()
    renderTag ancestors (tagNode, count) = do
      let tag = constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
          tit = show count <> " zettels tagged"
          cls = bool "" "inactive" $ count == 0
      divClass "node" $ do
        elAttr "a" ("class" =: cls <> "title" =: tit <> "href" =: tagUrl tag) $ do
          text $ renderTagNode tagNode
    renderTagNode :: NonEmpty TagNode -> Text
    renderTagNode = \case
      n :| (nonEmpty -> mrest) ->
        case mrest of
          Nothing ->
            unTagNode n
          Just rest ->
            unTagNode n <> "/" <> renderTagNode rest

-- TODO: not using Rib for ghcjs, but factorize this
zettelUrl :: ZettelID -> Text
zettelUrl zid =
  zettelIDText zid <> ".html"

tagUrl :: Tag -> Text
tagUrl (Tag s) =
  "search.html?tag=" <> s
