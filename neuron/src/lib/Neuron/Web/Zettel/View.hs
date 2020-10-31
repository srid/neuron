{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Zettel.View
  ( renderZettel,
    renderZettelContentCard,
    renderZettelParseError,
  )
where

import Data.Some
import Data.TagTree
import Data.Tagged (untag)
import Neuron.Reader.Type (ZettelParseError)
import qualified Neuron.Web.Query.View as Q
import Neuron.Web.Route
  ( NeuronWebT,
    Route (Route_Search),
    neuronRouteLink,
    whenStaticallyGenerated,
  )
import Neuron.Web.Widget (elPreOverflowing, elTime)
import qualified Neuron.Web.Widget.AutoScroll as AS
import qualified Neuron.Web.Widget.InvertedTree as IT
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import qualified Neuron.Zettelkasten.Query.Eval as Q
import qualified Neuron.Zettelkasten.Query.Parser as Q
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelC,
    ZettelT (..),
    sansContent,
  )
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc
  ( Config (Config),
    PandocBuilder,
    elPandoc,
  )
import Relude hiding ((&))
import Text.Pandoc.Definition (Pandoc (Pandoc))
import qualified Text.URI as URI

renderZettel ::
  PandocBuilder t m =>
  (ZettelGraph, ZettelC) ->
  NeuronWebT t m ()
renderZettel (graph, zc@(sansContent -> z)) = do
  let upTree = G.backlinkForest Folgezettel z graph
  unless (null upTree) $ do
    IT.renderInvertedHeadlessTree "zettel-uptree" "deemphasized" upTree $ \z2 ->
      Q.renderZettelLink Nothing (fmap fst $ G.getConnection z z2 graph) def z2
  -- Main content
  elAttr "div" ("class" =: "ui text container" <> "id" =: "zettel-container" <> "style" =: "position: relative") $ do
    whenStaticallyGenerated $ do
      -- We use -24px (instead of -14px) here so as to not scroll all the way to
      -- title, and as to leave some of the tree visible as "hint" to the user.
      lift $ AS.marker "zettel-container-anchor" (-24)
    divClass "zettel-view" $ do
      renderZettelContentCard (graph, zc)
      renderZettelBottomPane graph z
  -- Because the tree above can be pretty large, we scroll past it
  -- automatically when the page loads.
  whenStaticallyGenerated $ do
    unless (null upTree) $ do
      AS.script "zettel-container-anchor"

renderZettelContentCard ::
  PandocBuilder t m =>
  (ZettelGraph, ZettelC) ->
  NeuronWebT t m ()
renderZettelContentCard (graph, zc) =
  case zc of
    Right z -> do
      renderZettelContent (mkPandocRenderConfig graph) z
    Left z -> do
      renderZettelRawContent z

renderZettelBottomPane :: forall t m. PandocBuilder t m => ZettelGraph -> Zettel -> NeuronWebT t m ()
renderZettelBottomPane graph z@Zettel {..} = do
  let backlinks = nonEmpty $ G.backlinks isJust z graph
      tags = nonEmpty zettelTags
  when (isJust backlinks || isJust tags) $
    elClass "nav" "ui bottom attached segment deemphasized" $ do
      whenJust backlinks $ \links -> do
        elClass "h3" "ui header" $ text "Backlinks"
        elClass "ul" "backlinks" $ do
          forM_ links $ \((conn, ctxList), zl) ->
            el "li" $ do
              Q.renderZettelLink Nothing (Just conn) def zl
              elAttr "ul" ("class" =: "context-list" <> "style" =: "zoom: 85%;") $ do
                forM_ ctxList $ \ctx -> do
                  elClass "li" "item" $ do
                    void $ elPandoc (mkPandocRenderConfig graph) $ Pandoc mempty [ctx]
      whenJust tags $
        renderTags

mkPandocRenderConfig ::
  PandocBuilder t m =>
  ZettelGraph ->
  Config t (NeuronWebT t m) [QueryResultError]
mkPandocRenderConfig graph =
  Config $ \oldRender (URI.mkURI -> muri) minner -> do
    case muri of
      Nothing ->
        oldRender
      Just (Q.parseQueryLink -> mquery) -> do
        case mquery of
          Nothing ->
            -- This is not a query link; pass through.
            oldRender
          Just query ->
            case Q.runQuery (G.getZettels graph) query of
              Left e@(QueryResultError_NoSuchZettel mconn zid) -> do
                Q.renderMissingZettelLink mconn zid
                pure [e]
              Right res -> do
                Q.renderQueryResult minner res
                pure mempty

renderZettelContent ::
  forall t m.
  (PandocBuilder t m) =>
  Config t (NeuronWebT t m) [QueryResultError] ->
  ZettelT Pandoc ->
  NeuronWebT t m ()
renderZettelContent renderCfg Zettel {..} = do
  elClass "article" "ui raised attached segment zettel-content" $ do
    unless zettelTitleInBody $ do
      el "h1" $ text zettelTitle
    void $ elPandoc renderCfg zettelContent
    whenJust zettelDate $ \date ->
      divClass "metadata" $ do
        elAttr "div" ("class" =: "date" <> "title" =: "Zettel date") $ do
          elTime date

renderZettelRawContent :: (DomBuilder t m) => ZettelT Text -> m ()
renderZettelRawContent Zettel {..} = do
  divClass "ui error message" $ do
    elClass "h2" "header" $ text "Zettel failed to parse"
    maybe blank renderZettelParseError zettelError
  elClass "article" "ui raised attached segment zettel-content raw" $ do
    elPreOverflowing $ text $ zettelContent

renderZettelParseError :: DomBuilder t m => ZettelParseError -> m ()
renderZettelParseError err =
  el "p" $ elPreOverflowing $ text $ untag err

renderTags :: DomBuilder t m => NonEmpty Tag -> NeuronWebT t m ()
renderTags tags = do
  forM_ tags $ \t -> do
    -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
    -- the top pushes the zettel content down, introducing unnecessary white
    -- space below the title. So we put it at the bottom for now.
    neuronRouteLink
      (Some $ Route_Search $ Just t)
      ( "class" =: "ui right ribbon label zettel-tag "
          <> "title" =: ("See all zettels tagged '" <> unTag t <> "'")
      )
      $ text $ unTag t
    el "p" blank
