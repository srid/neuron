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

-- | Zettel ID
module Neuron.Zettelkasten.Link where

import Lucid
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Route
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Type
import Relude
import qualified Rib
import qualified Text.MMark.Extension as Ext
import Text.MMark.Extension (Extension, Inline (..))
import qualified Text.URI as URI

-- | MMark extension to transform `z:/` links in Markdown
zettelLinkExt :: ZettelStore -> Extension
zettelLinkExt store =
  mmarkCustomUrlProtocolRenderExt linkSchemes $ \inner _uri -> do
    let zid = parseZettelID $ Ext.asPlainText inner
    renderZettelLink LinkTheme_Default store zid
  where
    linkSchemes = connectionScheme <$> [minBound .. maxBound]
    mmarkCustomUrlProtocolRenderExt ::
      [URI.RText 'URI.Scheme] ->
      (NonEmpty Inline -> URI.URI -> Html ()) ->
      Extension
    mmarkCustomUrlProtocolRenderExt protos render =
      Ext.inlineRender $ \f -> \case
        inline@(Link inner uri _title) ->
          if or (((URI.uriScheme uri ==) . Just) <$> protos)
            then render inner uri
            else f inline
        inline ->
          f inline

data LinkTheme
  = LinkTheme_Default
  | LinkTheme_Menu ZettelID
  deriving (Eq, Show, Ord)

renderZettelLink :: Monad m => LinkTheme -> ZettelStore -> ZettelID -> HtmlT m ()
renderZettelLink ltheme store zid = do
  let Zettel {..} = lookupStore zid store
      zurl = Rib.routeUrlRel $ Route_Zettel zid
  case ltheme of
    LinkTheme_Default -> do
      span_ [class_ "zettel-link"] $ do
        span_ [class_ "zettel-link-idlink"] $ do
          a_ [href_ zurl] $ toHtml zid
        span_ [class_ "zettel-link-title"] $ do
          toHtml $ zettelTitle
    LinkTheme_Menu ignoreZid -> do
      if zid == ignoreZid
        then div_ [class_ "zettel-link item active", title_ $ unZettelID zid] $ do
          span_ [class_ "zettel-link-title"] $ do
            b_ $ toHtml zettelTitle
        else a_ [class_ "zettel-link item", href_ zurl, title_ $ unZettelID zid] $ do
          span_ [class_ "zettel-link-title"] $ do
            toHtml zettelTitle
