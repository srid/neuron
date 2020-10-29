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
import Neuron.Web.Widget
import qualified Neuron.Web.Widget.AutoScroll as AS
import qualified Neuron.Web.Widget.InvertedTree as IT
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import qualified Neuron.Zettelkasten.Query.Eval as Q
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc
import Relude hiding ((&))
import Text.Pandoc.Definition (Inline, Pandoc)
import qualified Text.URI as URI

renderZettel ::
  PandocBuilder t m =>
  (ZettelGraph, ZettelC) ->
  NeuronWebT t m ()
renderZettel (graph, zc@(sansContent -> z)) = do
  let upTree = G.backlinkForest Folgezettel z graph
  unless (null upTree) $ do
    IT.renderInvertedHeadlessTree "zettel-uptree" "deemphasized" upTree $ \z2 ->
      Q.renderZettelLink Nothing (G.getConnection z z2 graph) def z2
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
    Right z ->
      renderZettelContent (evalAndRenderZettelQuery graph) z
    Left z -> do
      renderZettelRawContent z

renderZettelBottomPane :: DomBuilder t m => ZettelGraph -> Zettel -> NeuronWebT t m ()
renderZettelBottomPane graph z@Zettel {..} = do
  let backlinks = nonEmpty $ G.backlinks isJust z graph
      tags = nonEmpty zettelTags
  when (isJust backlinks || isJust tags) $
    elClass "nav" "ui bottom attached segment deemphasized" $
      do
        divClass "ui grid" $ do
          divClass "fourteen wide column" $ do
            whenJust backlinks $ \links -> do
              elAttr "div" ("class" =: "ui header" <> "title" =: "Zettels that link here (branching or not)") $
                text "Backlinks"
              el "ul" $ do
                forM_ links $ \(conn, zl) ->
                  el "li" $ Q.renderZettelLink Nothing (Just conn) def zl
          whenJust tags $
            divClass "two wide column" . renderTags

evalAndRenderZettelQuery ::
  PandocBuilder t m =>
  ZettelGraph ->
  NeuronWebT t m [QueryResultError] ->
  Text ->
  Maybe [Inline] ->
  NeuronWebT t m [QueryResultError]
evalAndRenderZettelQuery graph oldRender lUrl minner = do
  case URI.mkURI lUrl of
    Nothing ->
      oldRender
    Just uri -> do
      case flip runReaderT (G.getZettels graph) (Q.runQueryURILink uri) of
        Left e@(QueryResultError_NoSuchZettel mconn zid) -> do
          Q.renderMissingZettelLink mconn zid
          pure [e]
        Right Nothing -> do
          -- This is not a query link; pass through.
          oldRender
        Right (Just res) -> do
          Q.renderQueryResult minner res
          pure mempty

renderZettelContent ::
  forall t m a.
  (PandocBuilder t m, Monoid a) =>
  (NeuronWebT t m a -> Text -> Maybe [Inline] -> NeuronWebT t m a) ->
  ZettelT Pandoc ->
  NeuronWebT t m ()
renderZettelContent handleLink Zettel {..} = do
  elClass "article" "ui raised attached segment zettel-content" $ do
    unless zettelTitleInBody $ do
      el "h1" $ text zettelTitle
    void $ elPandoc (Config handleLink) zettelContent
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
    elAttr "span" ("class" =: "ui right ribbon label zettel-tag" <> "title" =: "Tag") $ do
      neuronRouteLink
        (Some $ Route_Search $ Just t)
        ( "class" =: "tag-inner"
            <> "title" =: ("See all zettels tagged '" <> unTag t <> "'")
        )
        $ do
          text $ unTag t
    el "p" blank
