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

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link.View
  ( neuronLinkExt,
    renderZettelLink,
  )
where

import qualified Data.Map.Strict as Map
import Data.Some
import Data.Tree
import Lucid
import Neuron.Web.Route (Route (..), routeUrlRelWithQuery)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link
import Neuron.Zettelkasten.Link.Theme
import Neuron.Zettelkasten.Markdown (MarkdownLink (..))
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Tag (Tag (..), TagNode (..), constructTag, foldTagTree, tagMatchAny, tagTree)
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib
import qualified Text.MMark.Extension as Ext
import Text.MMark.Extension (Extension, Inline (..))
import Text.URI.QQ (queryKey)

-- | MMark extension to transform neuron links to custom views
neuronLinkExt :: HasCallStack => [Zettel] -> Extension
neuronLinkExt zettels =
  Ext.inlineRender $ \f -> \case
    inline@(Link inner uri _title) ->
      let mlink = MarkdownLink (Ext.asPlainText inner) uri
       in case neuronLinkFromMarkdownLink mlink of
            Right (Just nl) ->
              renderNeuronLink zettels nl
            Right Nothing ->
              f inline
            Left e ->
              -- TODO: Build the links during graph construction, and pass it to
              -- the extension from rendering stage. This way we don't have to
              -- re-parse the URIs and needlessly handle the errors.
              error $ show e
    inline ->
      f inline

-- | Render the custom view for the given neuron link
renderNeuronLink :: forall m. (Monad m, HasCallStack) => [Zettel] -> NeuronLink -> HtmlT m ()
renderNeuronLink zettels = \case
  NeuronLink (q@(Query_ZettelByID _zid), _conn, linkTheme) ->
    -- Render a single link
    case runQuery zettels q of
      Nothing ->
        -- TODO: See the TODO above
        error "No zettel for that ID"
      Just zettel ->
        renderZettelLink linkTheme zettel
  NeuronLink (q@(Query_ZettelsByTag pats), _conn, ZettelsView {..}) -> do
    let matches = sortZettelsReverseChronological $ runQuery zettels q
    toHtml $ Some q
    case zettelsViewGroupByTag of
      False ->
        -- Render a list of links
        renderZettelLinks zettelsViewLinkTheme matches
      True ->
        forM_ (Map.toList $ groupZettelsByTagsMatching pats matches) $ \(tag, zettelGrp) -> do
          span_ [class_ "ui basic pointing below grey label"] $ do
            i_ [class_ "tag icon"] mempty
            toHtml $ unTag tag
          renderZettelLinks zettelsViewLinkTheme zettelGrp
  NeuronLink (q@(Query_Tags _), (), ()) -> do
    -- Render a list of tags
    toHtml $ Some q
    renderTagTree $ foldTagTree $ tagTree $ runQuery zettels q
  where
    sortZettelsReverseChronological =
      sortOn (Down . zettelIDDay . zettelID)
    groupZettelsByTagsMatching pats matches =
      fmap sortZettelsReverseChronological $ Map.fromListWith (<>) $ flip concatMap matches $ \z ->
        flip concatMap (zettelTags z) $ \t -> [(t, [z]) | tagMatchAny pats t]
    renderZettelLinks :: LinkTheme -> [Zettel] -> HtmlT m ()
    renderZettelLinks ltheme zs =
      ul_ $ do
        forM_ zs $ \z ->
          li_ $ renderZettelLink ltheme z

-- | Render a link to an individual zettel.
renderZettelLink :: forall m. Monad m => LinkTheme -> Zettel -> HtmlT m ()
renderZettelLink ltheme Zettel {..} = do
  let zurl = Rib.routeUrlRel $ Route_Zettel zettelID
      renderDefault :: ToHtml a => a -> HtmlT m ()
      renderDefault linkInline = do
        span_ [class_ "zettel-link"] $ do
          span_ [class_ "zettel-link-idlink"] $ do
            a_ [href_ zurl] $ toHtml linkInline
          span_ [class_ "zettel-link-title"] $ do
            toHtml zettelTitle
  case ltheme of
    LinkTheme_Default ->
      -- Special consistent styling for Zettel links
      -- Uses ZettelID as link text. Title is displayed aside.
      renderDefault zettelID
    LinkTheme_WithDate ->
      case zettelIDDay zettelID of
        Just day ->
          renderDefault $ show @Text day
        Nothing ->
          -- Fallback to using zid
          renderDefault zettelID
    LinkTheme_Simple ->
      renderZettelLinkSimpleWith zurl (zettelIDText zettelID) zettelTitle

-- | Render a normal looking zettel link with a custom body.
renderZettelLinkSimpleWith :: forall m a. (Monad m, ToHtml a) => Text -> Text -> a -> HtmlT m ()
renderZettelLinkSimpleWith url title body =
  a_ [class_ "zettel-link item", href_ url, title_ title] $ do
    span_ [class_ "zettel-link-title"] $ do
      toHtml body

-- |  Render a tag tree along with the count of zettels tagged with it
renderTagTree :: forall m. Monad m => Forest (NonEmpty TagNode, Natural) -> HtmlT m ()
renderTagTree t =
  div_ [class_ "tag-tree"] $
    renderForest mempty t
  where
    renderForest :: [TagNode] -> Forest (NonEmpty TagNode, Natural) -> HtmlT m ()
    renderForest ancestors forest =
      ul_ $ do
        forM_ forest $ \tree -> do
          li_ $ renderTree ancestors tree
    renderTree :: [TagNode] -> Tree (NonEmpty TagNode, Natural) -> HtmlT m ()
    renderTree ancestors (Node (tagNode, count) children) = do
      renderTag ancestors (tagNode, count)
      whenNotNull children $
        renderForest (ancestors <> toList tagNode) . toList
    renderTag :: [TagNode] -> (NonEmpty TagNode, Natural) -> HtmlT m ()
    renderTag ancestors (tagNode, count) = do
      div_ [class_ "node"] $ do
        let Tag tag = constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
            attrs = case count of
              0 ->
                [class_ "inactive"]
              _ ->
                [ href_ $ routeUrlRelWithQuery Route_Search [queryKey|tag|] tag,
                  title_ $ show count <> " zettels tagged"
                ]
        a_ attrs $ renderTagNode tagNode
    renderTagNode :: NonEmpty TagNode -> HtmlT m ()
    renderTagNode = \case
      n :| (nonEmpty -> mrest) ->
        case mrest of
          Nothing ->
            toHtml $ unTagNode n
          Just rest -> do
            toHtml $ unTagNode n
            "/"
            renderTagNode rest
