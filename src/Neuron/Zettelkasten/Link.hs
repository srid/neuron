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
module Neuron.Zettelkasten.Link where

import Lucid
import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Meta as Meta
import Neuron.Zettelkasten.Route
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Type
import Relude
import qualified Rib
import qualified Text.MMark.Extension as Ext
import Text.MMark.Extension (Extension, Inline (..))
import qualified Text.URI as URI

data LinkAction
  = LinkAction_ConnectZettel Connection
  deriving (Eq, Show, Bounded)

linkActionFromUri :: URI.URI -> Maybe LinkAction
linkActionFromUri uri = case fmap URI.unRText (URI.uriScheme uri) of
  Just "z" -> Just $ LinkAction_ConnectZettel Folgezettel
  Just "zcf" -> Just $ LinkAction_ConnectZettel OrdinaryConnection
  _ -> Nothing

linkActionRender :: Monad m => ZettelStore -> NonEmpty Inline -> URI.URI -> LinkAction -> HtmlT m ()
linkActionRender store inner _uri = \case
  LinkAction_ConnectZettel _conn -> do
    -- The inner link text is supposed to be the zettel ID
    let zid = parseZettelID $ Ext.asPlainText inner
    renderZettelLink LinkTheme_Default store zid

-- | MMark extension to transform `z:/` links in Markdown
linkActionExt :: ZettelStore -> Extension
linkActionExt store =
  Ext.inlineRender $ \f -> \case
    inline@(Link inner uri _title) ->
      case linkActionFromUri uri of
        Just lact ->
          linkActionRender store inner uri lact
        Nothing ->
          f inline
    inline ->
      f inline

data LinkTheme
  = LinkTheme_Default
  | LinkTheme_Menu (Maybe ZettelID)
  deriving (Eq, Show, Ord)

renderZettelLink :: Monad m => LinkTheme -> ZettelStore -> ZettelID -> HtmlT m ()
renderZettelLink ltheme store zid = do
  let Zettel {..} = lookupStore zid store
      zurl = Rib.routeUrlRel $ Route_Zettel zid
      ztagsM = Meta.tags =<< Meta.getMeta zettelContent
  case ltheme of
    LinkTheme_Default -> do
      -- Special consistent styling for Zettel links
      -- Uses ZettelID as link text. Title is displayed aside.
      span_ [class_ "zettel-link"] $ do
        span_ [class_ "zettel-link-idlink"] $ do
          a_ [href_ zurl] $ toHtml zid
        span_ [class_ "zettel-link-title"] $ do
          toHtml $ zettelTitle
        whenJust ztagsM $ \ztags ->
          forM_ ztags $ \ztag ->
            span_ [class_ "ui label"] $ toHtml ztag
    LinkTheme_Menu ignoreZid -> do
      -- A normal looking link.
      -- Zettel's title is the link text.
      if Just zid == ignoreZid
        then div_ [class_ "zettel-link item active", title_ $ unZettelID zid] $ do
          span_ [class_ "zettel-link-title"] $ do
            b_ $ toHtml zettelTitle
        else a_ [class_ "zettel-link item", href_ zurl, title_ $ unZettelID zid] $ do
          span_ [class_ "zettel-link-title"] $ do
            toHtml zettelTitle
