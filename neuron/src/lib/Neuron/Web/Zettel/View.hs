{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Zettel.View
  ( renderZettel,
    renderZettelContentCard,
  )
where

import Data.TagTree
import qualified Neuron.Web.Query.View as Q
import Neuron.Web.Route
import Neuron.Web.Widget
import qualified Neuron.Web.Widget.AutoScroll as AS
import qualified Neuron.Web.Widget.InvertedTree as IT
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Query.Error (QueryError, showQueryError)
import qualified Neuron.Zettelkasten.Query.Eval as Q
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc
import Relude hiding ((&))
import Text.Pandoc.Definition (Pandoc)

renderZettel ::
  PandocBuilder t m =>
  (ZettelGraph, PandocZettel) ->
  NeuronWebT t m [QueryError]
renderZettel (graph, pz@(PandocZettel (z@Zettel {..}, _))) = do
  let upTree = G.backlinkForest Folgezettel z graph
  unless (null upTree) $ do
    IT.renderInvertedHeadlessTree "zettel-uptree" "deemphasized" upTree $ \z2 ->
      Q.renderZettelLink (G.getConnection z z2 graph) def z2
  -- Main content
  errors <- elAttr "div" ("class" =: "ui text container" <> "id" =: "zettel-container" <> "style" =: "position: relative") $ do
    whenStaticallyGenerated $ do
      -- We use -24px (instead of -14px) here so as to not scroll all the way to
      -- title, and as to leave some of the tree visible as "hint" to the user.
      lift $ AS.marker "zettel-container-anchor" (-24)
    divClass "zettel-view" $ do
      errors <-
        renderZettelContentCard (graph, pz)
          <* renderZettelBottomPane graph z
      pure errors
  -- Because the tree above can be pretty large, we scroll past it
  -- automatically when the page loads.
  whenStaticallyGenerated $ do
    unless (null upTree) $ do
      AS.script "zettel-container-anchor"
  pure errors

renderZettelContentCard ::
  PandocBuilder t m =>
  (ZettelGraph, PandocZettel) ->
  NeuronWebT t m [QueryError]
renderZettelContentCard (graph, (PandocZettel (z@Zettel {..}, zc))) =
  renderZettelContent (evalAndRenderZettelQuery graph) z zc

renderZettelBottomPane :: DomBuilder t m => ZettelGraph -> Zettel -> NeuronWebT t m ()
renderZettelBottomPane graph z@Zettel {..} = do
  let cfBacklinks = nonEmpty $ G.backlinks OrdinaryConnection z graph
      tags = nonEmpty zettelTags
  when (isJust cfBacklinks || isJust tags)
    $ elClass "nav" "ui bottom attached segment deemphasized"
    $ do
      divClass "ui two column grid" $ do
        divClass "column" $ do
          whenJust cfBacklinks $ \links -> do
            elAttr "div" ("class" =: "ui header" <> "title" =: "Zettels that link here, but without branching") $
              text "More backlinks"
            el "ul" $ do
              forM_ links $ \zl ->
                el "li" $ Q.renderZettelLink Nothing def zl
        whenJust tags $
          divClass "column" . renderTags

evalAndRenderZettelQuery ::
  PandocBuilder t m =>
  ZettelGraph ->
  NeuronWebT t m [QueryError] ->
  URILink ->
  NeuronWebT t m [QueryError]
evalAndRenderZettelQuery graph oldRender uriLink = do
  case flip runReaderT (G.getZettels graph) (Q.runQueryURILink uriLink) of
    Left (Left -> e) -> do
      -- Error parsing the query.
      fmap (e :) oldRender <* elInlineError e
    Right Nothing -> do
      -- This is not a query link; pass through.
      oldRender
    Right (Just res) -> do
      Q.renderQueryResultIfSuccessful res >>= \case
        Nothing ->
          -- Successfully rendered.
          pure mempty
        Just (Right -> e) -> do
          -- Error in query results.
          fmap (e :) oldRender <* elInlineError e
  where
    elInlineError e =
      elClass "span" "ui left pointing red basic label" $ do
        text $ showQueryError e

renderZettelContent ::
  forall t m a.
  (PandocBuilder t m, Monoid a) =>
  (NeuronWebT t m a -> URILink -> NeuronWebT t m a) ->
  Zettel ->
  Pandoc ->
  NeuronWebT t m a
renderZettelContent handleLink Zettel {..} doc = do
  elClass "article" "ui raised attached segment zettel-content" $ do
    elClass "h1" "header" $ text zettelTitle
    x <- elPandoc (Config handleLink) doc
    whenJust zettelDay $ \day ->
      elAttr "div" ("class" =: "date" <> "title" =: "Zettel creation date") $ do
        elTime day
    pure x

renderTags :: DomBuilder t m => NonEmpty Tag -> m ()
renderTags tags = do
  forM_ tags $ \t -> do
    -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
    -- the top pushes the zettel content down, introducing unnecessary white
    -- space below the title. So we put it at the bottom for now.
    elAttr "span" ("class" =: "ui right ribbon label zettel-tag" <> "title" =: "Tag") $ do
      elAttr
        "a"
        ( "href" =: (Q.tagUrl t)
            <> "class" =: "tag-inner"
            <> "title" =: ("See all zettels tagged '" <> unTag t <> "'")
        )
        $ text
        $ unTag t
    el "p" blank
