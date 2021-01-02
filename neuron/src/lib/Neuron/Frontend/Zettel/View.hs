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

module Neuron.Frontend.Zettel.View
  ( renderZettel,
    renderZettelContentCard,
    renderZettelParseError,
    renderBottomMenu,
  )
where

import Control.Monad.Fix (MonadFix)
import qualified Data.Dependent.Map as DMap
import Data.List (maximum)
import Data.Some (Some (Some))
import Data.TagTree (Tag (unTag))
import Data.Tagged (untag)
import qualified Data.Tree as Tree
import qualified Neuron.Frontend.Query.View as Q
import Neuron.Frontend.Route
  ( NeuronWebT,
    Route (..),
    neuronDynRouteLink,
    neuronRouteLink,
  )
import Neuron.Frontend.Theme (Theme)
import qualified Neuron.Frontend.Theme as Theme
import Neuron.Frontend.Widget (elPreOverflowing, elTime, semanticIcon)
import qualified Neuron.Frontend.Widget.AutoScroll as AS
import qualified Neuron.Frontend.Widget.InvertedTree as IT
import Neuron.Markdown (ZettelParseError)
import Neuron.Plugin (renderPluginPanel)
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (indexZid)
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import qualified Neuron.Zettelkasten.Query.Eval as Q
import qualified Neuron.Zettelkasten.Query.Parser as Q
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelC,
    ZettelT (..),
    sansContent,
  )
import qualified Neuron.Zettelkasten.Zettel as Z
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
  (PandocBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  Theme ->
  (ZettelGraph, ZettelC) ->
  Maybe Text ->
  NeuronWebT t m ()
renderZettel theme (graph, zc@(sansContent -> z)) mEditUrl = do
  -- Open impulse on pressing the forward slash key.
  el "script" $ do
    text "document.onkeyup = function(e) { if ([\"/\", \"s\"].includes(e.key)) { document.location.href = \"impulse.html\"; } }"
  let upTree = G.backlinkForest Folgezettel z graph
  unless (null upTree) $ do
    IT.renderInvertedHeadlessTree "zettel-uptree" "deemphasized" upTree $ \z2 ->
      Q.renderZettelLink Nothing (fst <$> G.getConnection z z2 graph) def z2
  -- Main content
  elAttr "div" ("class" =: "ui text container" <> "id" =: "zettel-container" <> "style" =: "position: relative") $ do
    -- We use -24px (instead of -14px) here so as to not scroll all the way to
    -- title, and as to leave some of the tree visible as "hint" to the user.
    lift $ AS.marker "zettel-container-anchor" (-24)
    divClass "zettel-view" $ do
      renderZettelContentCard (graph, zc)
      forM_ (DMap.toList $ zettelPluginData z) $ \pluginData ->
        renderPluginPanel graph pluginData
      renderZettelBottomPane graph z
      renderBottomMenu (constDyn theme) (constDyn $ G.getZettel indexZid graph) ((<> toText (zettelPath z)) <$> mEditUrl)
  -- Because the tree above can be pretty large (4+ height), we scroll past it
  -- automatically when the page loads.
  when (forestDepth upTree > 3) $
    AS.script "zettel-container-anchor"
  where
    forestDepth :: Tree.Forest a -> Int
    forestDepth = \case
      [] -> 0
      ts -> maximum $ fmap (length . Tree.levels) ts

renderZettelContentCard ::
  (PandocBuilder t m, PostBuild t m) =>
  (ZettelGraph, ZettelC) ->
  NeuronWebT t m ()
renderZettelContentCard (graph, zc) =
  case zc of
    Right z -> do
      renderZettelContent (mkPandocRenderConfig graph) z
    Left z -> do
      renderZettelRawContent z

renderZettelBottomPane ::
  (PandocBuilder t m, PostBuild t m) =>
  ZettelGraph ->
  Zettel ->
  NeuronWebT t m ()
renderZettelBottomPane graph z@Zettel {..} = do
  let backlinks = nonEmpty $ G.backlinks isJust z graph
      tags = nonEmpty $ toList zettelTags
  whenJust (() <$ backlinks <|> () <$ tags) $ \() -> do
    elClass "nav" "ui attached segment deemphasized bottomPane" $ do
      -- Backlinks
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
      -- Tags
      whenJust tags renderTags

renderBottomMenu ::
  (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) =>
  Dynamic t Theme ->
  -- | "Home" link
  Dynamic t (Maybe Zettel) ->
  -- | "Edit" URL for this route
  Maybe Text ->
  NeuronWebT t m ()
renderBottomMenu themeDyn mIndexZettel mEditUrl = do
  let divAttrs = ffor themeDyn $ \theme ->
        "class" =: ("ui bottom attached icon compact inverted menu " <> Theme.semanticColor theme)
  elDynAttr "div" divAttrs $ do
    -- Home
    x <- maybeDyn mIndexZettel
    dyn_ $
      ffor x $ \case
        Nothing -> blank
        Just indexZettel -> do
          neuronDynRouteLink (Some . Route_Zettel . Z.zettelSlug <$> indexZettel) ("class" =: "item" <> "title" =: "Home") $
            semanticIcon "home"
    -- Edit url
    forM_ mEditUrl $ \editUrl -> do
      let attrs = "href" =: editUrl <> "title" =: "Edit this page"
      elAttr "a" ("class" =: "item" <> attrs) $ do
        semanticIcon "edit"
    -- Impulse
    neuronRouteLink (Some $ Route_Impulse Nothing) ("class" =: "right item" <> "title" =: "Open Impulse (press /)") $ do
      semanticIcon "wave square"

mkPandocRenderConfig ::
  (PandocBuilder t m, PostBuild t m) =>
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

renderZettelRawContent :: DomBuilder t m => ZettelT (Text, ZettelParseError) -> m ()
renderZettelRawContent Zettel {..} = do
  divClass "ui error message" $ do
    elClass "h2" "header" $ text "Zettel failed to parse"
    renderZettelParseError $ snd zettelContent
  elClass "article" "ui raised attached segment zettel-content raw" $ do
    elPreOverflowing $ text $ fst zettelContent

renderZettelParseError :: DomBuilder t m => ZettelParseError -> m ()
renderZettelParseError err =
  el "p" $ elPreOverflowing $ text $ untag err

renderTags :: (DomBuilder t m, PostBuild t m) => NonEmpty Tag -> NeuronWebT t m ()
renderTags tags = do
  el "div" $ do
    forM_ tags $ \t -> do
      -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
      -- the top pushes the zettel content down, introducing unnecessary white
      -- space below the title. So we put it at the bottom for now.
      neuronRouteLink
        (Some $ Route_Impulse $ Just t)
        ( "class" =: "ui basic label zettel-tag"
            <> "title" =: ("See all zettels tagged '" <> unTag t <> "'")
        )
        $ text $ unTag t
