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
import qualified Data.Map.Strict as Map
import Data.Some (Some (Some))
import Data.TagTree (Tag (unTag))
import Data.Tagged (untag)
import qualified Data.Tree as Tree
import qualified Neuron.Frontend.Query.View as Q
import Neuron.Frontend.Route (NeuronWebT, Route (..))
import qualified Neuron.Frontend.Route as R
import Neuron.Frontend.Route.Data.Types (SiteData, ZettelData)
import qualified Neuron.Frontend.Route.Data.Types as R
import Neuron.Frontend.Theme (Theme)
import qualified Neuron.Frontend.Theme as Theme
import Neuron.Frontend.Widget (elPreOverflowing, elTime, semanticIcon)
import qualified Neuron.Frontend.Widget.AutoScroll as AS
import qualified Neuron.Frontend.Widget.InvertedTree as IT
import Neuron.Markdown (ZettelParseError)
import Neuron.Plugin (renderPluginPanel)
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Query.Eval (QueryUrlCache)
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

-- TODO:L Get rid of graph argument which is only used to:
-- - lookup link queries in Pandoc docs
-- - backlinks and uptree data
-- - plugin data (plugin route data)
renderZettel ::
  (PandocBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  SiteData ->
  ZettelData ->
  NeuronWebT t m ()
renderZettel siteData zData = do
  -- Open impulse on pressing the forward slash key.
  el "script" $ do
    text "document.onkeyup = function(e) { if ([\"/\", \"s\"].includes(e.key)) { document.location.href = \"impulse.html\"; } }"
  let upTree = R.zettelDataUptree zData
  unless (null upTree) $ do
    IT.renderInvertedHeadlessTree "zettel-uptree" "deemphasized" upTree $ \z2 ->
      Q.renderZettelLink Nothing Nothing def z2
  -- Main content
  elAttr "div" ("class" =: "ui text container" <> "id" =: "zettel-container" <> "style" =: "position: relative") $ do
    -- We use -24px (instead of -14px) here so as to not scroll all the way to
    -- title, and as to leave some of the tree visible as "hint" to the user.
    lift $ AS.marker "zettel-container-anchor" (-24)
    let rdpConfig = mkReflexDomPandocConfig $ R.zettelDataQueryUrlCache zData
    divClass "zettel-view" $ do
      let zc = R.zettelDataZettel zData
          z = sansContent zc
          graph = R.zettelDataGraph zData
      renderZettelContentCard rdpConfig zc
      forM_ (DMap.toList $ zettelPluginData z) $ \pluginData ->
        renderPluginPanel graph pluginData
      renderZettelBottomPane graph rdpConfig z
      renderBottomMenu
        (constDyn $ R.siteDataTheme siteData)
        (constDyn $ R.siteDataIndexZettel siteData)
        ((<> toText (zettelPath z)) <$> R.siteDataEditUrl siteData)
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
  Config t (NeuronWebT t m) () ->
  ZettelC ->
  NeuronWebT t m ()
renderZettelContentCard rdpConfig zc =
  case zc of
    Right z -> do
      renderZettelContent rdpConfig z
    Left z -> do
      renderZettelRawContent z

renderZettelBottomPane ::
  (PandocBuilder t m, PostBuild t m) =>
  ZettelGraph ->
  Config t (NeuronWebT t m) () ->
  Zettel ->
  NeuronWebT t m ()
renderZettelBottomPane graph rdpConfig z@Zettel {..} = do
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
                    void $ elPandoc rdpConfig $ Pandoc mempty [ctx]
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
          R.neuronDynRouteLink (Some . Route_Zettel . Z.zettelSlug <$> indexZettel) ("class" =: "item" <> "title" =: "Home") $
            semanticIcon "home"
    -- Edit url
    forM_ mEditUrl $ \editUrl -> do
      let attrs = "href" =: editUrl <> "title" =: "Edit this page"
      elAttr "a" ("class" =: "item" <> attrs) $ do
        semanticIcon "edit"
    -- Impulse
    R.neuronRouteLink (Some $ Route_Impulse Nothing) ("class" =: "right item" <> "title" =: "Open Impulse (press /)") $ do
      semanticIcon "wave square"

mkReflexDomPandocConfig ::
  (PandocBuilder t m, PostBuild t m) =>
  QueryUrlCache ->
  Config t (NeuronWebT t m) ()
mkReflexDomPandocConfig qurlcache =
  Config $ \oldRender url minner ->
    fromMaybe oldRender $ do
      -- TODO: replace with rd cache
      qres <- Map.lookup url qurlcache
      pure $
        Q.renderQueryResult minner qres

renderZettelContent ::
  forall t m.
  (PandocBuilder t m) =>
  Config t (NeuronWebT t m) () ->
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
      R.neuronRouteLink
        (Some $ Route_Impulse $ Just t)
        ( "class" =: "ui basic label zettel-tag"
            <> "title" =: ("See all zettels tagged '" <> unTag t <> "'")
        )
        $ text $ unTag t
