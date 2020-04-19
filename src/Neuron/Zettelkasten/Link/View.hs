{-# LANGUAGE DataKinds #-}
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

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link.View
  ( neuronLinkExt,
    renderZettelLink,
  )
where

import qualified Data.Map.Strict as Map
import Data.Some
import Lucid
import Neuron.Web.Route (Route (..), routeUrlRelWithQuery)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link
import Neuron.Zettelkasten.Link.Theme
import Neuron.Zettelkasten.Markdown (MarkdownLink (..))
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Tag (Tag (unTag), tagMatchAny)
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib
import qualified Text.MMark.Extension as Ext
import Text.MMark.Extension (Extension, Inline (..))
import Text.URI.QQ (queryKey)

-- | MMark extension to transform neuron links to custom views
neuronLinkExt :: HasCallStack => ZettelStore -> Extension
neuronLinkExt store =
  Ext.inlineRender $ \f -> \case
    inline@(Link inner uri _title) ->
      let mlink = MarkdownLink (Ext.asPlainText inner) uri
       in case neuronLinkFromMarkdownLink mlink of
            Right (Just nl) ->
              renderNeuronLink store nl
            Right Nothing ->
              f inline
            Left e ->
              error e
    inline ->
      f inline

-- | Render the custom view for the given neuron link
renderNeuronLink :: forall m. Monad m => ZettelStore -> NeuronLink -> HtmlT m ()
renderNeuronLink store = \case
  NeuronLink (Query_ZettelByID zid, _conn, linkTheme) ->
    -- Render a single link
    renderZettelLink linkTheme $ lookupStore zid store
  NeuronLink (q@(Query_ZettelsByTag pats), _conn, ZettelsView {..}) -> do
    let zettels = sortZettelsReverseChronological $ runQuery store q
    toHtml $ Some q
    case zettelsViewGroupByTag of
      False ->
        -- Render a list of links
        renderZettelLinks zettelsViewLinkTheme zettels
      True ->
        forM_ (Map.toList $ groupZettelsByTagsMatching pats zettels) $ \(tag, zettelGrp) -> do
          span_ [class_ "ui basic pointing below grey label"] $ toHtml $ unTag tag
          renderZettelLinks zettelsViewLinkTheme zettelGrp
  NeuronLink (q@(Query_Tags _), (), ()) -> do
    -- Render a list of tags
    toHtml $ Some q
    let tags = runQuery store q
    ul_ $ do
      forM_ tags $ \(unTag -> tag) -> do
        let tagUrl = routeUrlRelWithQuery Route_Search [queryKey|tag|] tag
        li_ $ a_ [href_ tagUrl] $ toHtml tag
  where
    sortZettelsReverseChronological =
      sortOn (Down . zettelIDDay . zettelID)
    groupZettelsByTagsMatching pats zettels =
      fmap sortZettelsReverseChronological $ Map.fromListWith (<>) $ flip concatMap zettels $ \z ->
        flip concatMap (zettelTags z) $ \t -> [(t, [z]) | tagMatchAny pats t]
    renderZettelLinks :: LinkTheme -> [Zettel] -> HtmlT m ()
    renderZettelLinks ltheme zettels =
      ul_ $ do
        forM_ zettels $ \z ->
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
