{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Query.View
  ( renderQueryResult,
    renderZettelLink,
    renderZettelLinkIDOnly,
    renderMissingZettelLink,
    style,
  )
where

import Clay (Css, em, (?))
import qualified Clay as C
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import Data.Some (Some (..))
import Data.TagTree
  ( Tag (..),
    TagNode (..),
    TagPattern (..),
    constructTag,
    foldTagTree,
    tagMatchAny,
    tagTree,
  )
import qualified Data.Text as T
import Data.Tree (Forest, Tree (Node))
import GHC.Natural (intToNatural, naturalToInt)
import Neuron.Web.Route
  ( NeuronWebT,
    Route (..),
    neuronRouteLink,
  )
import Neuron.Web.Widget (elNoSnippetSpan, elTime, semanticIcon)
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.ID (Slug, ZettelID (), unZettelID)
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelQuery (..),
    ZettelT (..),
    sortZettelsReverseChronological,
  )
import Reflex.Dom.Core hiding (count, tag)
import Reflex.Dom.Pandoc (PandocBuilder, elPandocInlines)
import Relude
import Text.Pandoc.Definition (Inline)

-- | Render the query results.
renderQueryResult ::
  PandocBuilder t m => Maybe [Inline] -> DSum ZettelQuery Identity -> NeuronWebT t m ()
renderQueryResult minner = \case
  ZettelQuery_ZettelByID _zid conn :=> Identity target -> do
    renderZettelLink (elPandocInlines <$> minner) (Just conn) Nothing target
  q@(ZettelQuery_ZettelsByTag pats conn view) :=> Identity res -> do
    el "section" $ do
      renderQuery $ Some q
      if zettelsViewGroupByTag view
        then forM_ (Map.toList $ groupZettelsByTagsMatching pats res) $ \(tag, zettelGrp) -> do
          el "section" $ do
            elClass "span" "ui basic pointing below grey label" $ do
              semanticIcon "tag"
              text $ unTag tag
            el "ul" $
              forM_ zettelGrp $ \z ->
                el "li" $
                  renderZettelLink Nothing (Just conn) (Just $ zettelsViewLinkView view) z
        else el "ul" $
          forM_ (maybe id (take . naturalToInt) (zettelsViewLimit view) $ res) $ \z -> do
            el "li" $
              renderZettelLink Nothing (Just conn) (Just $ zettelsViewLinkView view) z
      elClass "div" "ui basic center aligned segment" $ do
        el "em" $ do
          let zettelCount = (intToNatural . length $ res)
              limitHint = maybe "" (\limit -> " (Displaying " <> (show $ min limit zettelCount) <> " out of " <> show zettelCount <> " zettels)") $ zettelsViewLimit view
          text $ limitHint
  q@(ZettelQuery_Tags _) :=> Identity res -> do
    el "section" $ do
      renderQuery $ Some q
      renderTagTree $ foldTagTree $ tagTree res
  ZettelQuery_TagZettel tag :=> Identity () ->
    renderInlineTag tag mempty $ do
      text "#"
      text $ unTag tag
  where
    -- TODO: Instead of doing this here, group the results in runQuery itself.
    groupZettelsByTagsMatching pats matches =
      fmap sortZettelsReverseChronological $
        Map.fromListWith (<>) $
          flip concatMap matches $ \z ->
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
          semanticIcon "tags"
          text qs
      Some (ZettelQuery_Tags []) ->
        text "All tags"
      Some (ZettelQuery_Tags (fmap unTagPattern -> pats)) -> do
        let qs = toText $ intercalate ", " pats
        text $ "Tags matching '" <> qs <> "'"
      Some (ZettelQuery_TagZettel _tag) -> do
        blank

-- | Render a link to an individual zettel.
renderZettelLink ::
  DomBuilder t m =>
  -- | Link inner text
  Maybe (m ()) ->
  -- | Connection type to display
  Maybe Connection ->
  -- | Link theme
  Maybe LinkView ->
  Zettel ->
  NeuronWebT t m ()
renderZettelLink mInner conn (fromMaybe def -> linkView) Zettel {..} = do
  let connClass = show <$> conn
      rawClass = const (Just "errors") =<< zettelParseError
      mextra =
        case linkView of
          LinkView_Default ->
            Nothing
          LinkView_ShowDate ->
            elTime <$> zettelDate
          LinkView_ShowID ->
            Just $ el "tt" $ text $ unZettelID zettelID
      classes :: [Text] = catMaybes $ [Just "zettel-link-container"] <> [connClass, rawClass]
  elClass "span" (T.intercalate " " classes) $ do
    forM_ mextra $ \extra ->
      elNoSnippetSpan ("class" =: "extra monoFont") $ do
        extra
        -- The extra space is so that double clicking on this extra text
        -- doesn't select the title next.
        text " "
    elAttr "span" ("class" =: "zettel-link" <> withTooltip linkTooltip) $ do
      let linkInnerHtml = fromMaybe (text zettelTitle) mInner
      neuronRouteLink (Some $ Route_Zettel zettelSlug) mempty linkInnerHtml
      elConnSuffix conn
  where
    -- If there is custom inner text, put zettel title in tooltip.
    -- Otherwise put tags if any.
    linkTooltip
      | isJust mInner = Just $ "Zettel: " <> zettelTitle
      | null zettelTags = Nothing
      | otherwise = Just $ "Tags: " <> T.intercalate "; " (unTag <$> zettelTags)
    withTooltip :: Maybe Text -> Map Text Text
    withTooltip = \case
      Nothing -> mempty
      Just s ->
        ( "data-tooltip" =: s
            <> "data-inverted" =: ""
            <> "data-position" =: "right center"
        )

elConnSuffix :: DomBuilder t m => Maybe Connection -> m ()
elConnSuffix mconn =
  case mconn of
    Just Folgezettel -> elNoSnippetSpan mempty $ do
      elAttr "sup" ("title" =: "Branching link (folgezettel)") $ text "ᛦ"
    _ -> pure mempty

-- TODO: Eventually refactor this function to reuse what's in renderZettelLink
renderMissingZettelLink :: DomBuilder t m => Maybe Connection -> ZettelID -> m ()
renderMissingZettelLink mconn zid = do
  let connClass = show <$> mconn
      classes :: [Text] = catMaybes $ [Just "zettel-link-container", Just "errors"] <> [connClass]
  elClass "span" (T.intercalate " " classes) $ do
    let errMsg = "Wiki-link does not refer to any existing zettel"
    elAttr "span" ("class" =: "zettel-link" <> "title" =: errMsg) $ do
      elAttr "a" mempty $ text $ unZettelID zid
      elConnSuffix mconn

-- | Like `renderZettelLink` but when we only have ID in hand.
renderZettelLinkIDOnly :: DomBuilder t m => ZettelID -> Slug -> NeuronWebT t m ()
renderZettelLinkIDOnly zid slug =
  elClass "span" "zettel-link-container" $ do
    elClass "span" "zettel-link" $ do
      neuronRouteLink (Some $ Route_Zettel slug) mempty $ text $ unZettelID zid

renderTagTree :: forall t m. DomBuilder t m => Forest (NonEmpty TagNode, Natural) -> NeuronWebT t m ()
renderTagTree t =
  divClass "tag-tree" $
    renderForest mempty t
  where
    renderForest :: [TagNode] -> Forest (NonEmpty TagNode, Natural) -> NeuronWebT t m ()
    renderForest ancestors forest =
      el "ul" $ do
        forM_ forest $ \tree ->
          el "li" $ renderTree ancestors tree
    renderTree :: [TagNode] -> Tree (NonEmpty TagNode, Natural) -> NeuronWebT t m ()
    renderTree ancestors (Node (tagNode, count) children) = do
      renderTag ancestors (tagNode, count)
      renderForest (ancestors <> toList tagNode) $ toList children
    renderTag :: [TagNode] -> (NonEmpty TagNode, Natural) -> NeuronWebT t m ()
    renderTag ancestors (tagNode, count) = do
      let tag = constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
          tit = show count <> " zettels tagged"
          cls = bool "" "inactive" $ count == 0
      divClass "node" $ do
        renderInlineTag tag ("class" =: cls <> "title" =: tit) $
          text $ renderTagNode tagNode
    renderTagNode :: NonEmpty TagNode -> Text
    renderTagNode = \case
      n :| (nonEmpty -> mrest) ->
        case mrest of
          Nothing ->
            unTagNode n
          Just rest ->
            unTagNode n <> "/" <> renderTagNode rest

renderInlineTag :: DomBuilder t m => Tag -> Map Text Text -> m () -> NeuronWebT t m ()
renderInlineTag tag = neuronRouteLink (Some $ Route_Search $ Just tag)

style :: Css
style = do
  zettelLinkCss
  "div.tag-tree" ? do
    "div.node" ? do
      C.fontWeight C.bold
      "a.inactive" ? do
        C.color "#555"

zettelLinkCss :: Css
zettelLinkCss = do
  "span.zettel-link-container span.zettel-link a" ? do
    C.fontWeight C.bold
    C.textDecoration C.none
  "span.zettel-link-container span.extra" ? do
    C.color C.auto
  "span.zettel-link-container.errors" ? do
    C.border C.solid (C.px 1) C.red
  "[data-tooltip]:after" ? do
    C.fontSize $ em 0.7
