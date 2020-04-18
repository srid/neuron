{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link.View where

import Lucid
import Neuron.Web.Route (Route (..))
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link.Action
import Neuron.Zettelkasten.Link.Theme (LinkTheme (..))
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib

linkActionRender :: Monad m => ZettelStore -> MarkdownLink -> LinkAction -> HtmlT m ()
linkActionRender store _ = \case
  LinkAction_ConnectZettel _conn zid -> do
    renderZettelLink LinkTheme_Default store zid
  LinkAction_QueryZettels _conn linkTheme q -> do
    toHtml q
    let zettels = reverse $ sort $ zettelID <$> runQuery store q
    ul_ $ do
      forM_ zettels $ \zid -> do
        li_ $ renderZettelLink linkTheme store zid

renderZettelLink :: forall m. Monad m => LinkTheme -> ZettelStore -> ZettelID -> HtmlT m ()
renderZettelLink ltheme store zid = do
  let Zettel {..} = lookupStore zid store
      zurl = Rib.routeUrlRel $ Route_Zettel zid
      renderDefault :: ToHtml a => a -> HtmlT m ()
      renderDefault linkInline = do
        span_ [class_ "zettel-link"] $ do
          span_ [class_ "zettel-link-idlink"] $ do
            a_ [href_ zurl] $ toHtml linkInline
          span_ [class_ "zettel-link-title"] $ do
            toHtml $ zettelTitle
  case ltheme of
    LinkTheme_Default -> do
      -- Special consistent styling for Zettel links
      -- Uses ZettelID as link text. Title is displayed aside.
      renderDefault zid
    LinkTheme_WithDate -> do
      case zettelIDDay zid of
        Just day ->
          renderDefault $ show @Text day
        Nothing ->
          -- Fallback to using zid
          renderDefault zid
    LinkTheme_Simple -> do
      renderZettelLinkSimpleWith zurl (zettelIDText zid) zettelTitle

-- | Render a normal looking zettel link with a custom body.
renderZettelLinkSimpleWith :: forall m a. (Monad m, ToHtml a) => Text -> Text -> a -> HtmlT m ()
renderZettelLinkSimpleWith url title body =
  a_ [class_ "zettel-link item", href_ url, title_ title] $ do
    span_ [class_ "zettel-link-title"] $ do
      toHtml body
