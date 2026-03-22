{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Some (Some (Some))
import Data.Tagged (untag)
import Neuron.Frontend.Route (NeuronWebT, Route (..))
import qualified Neuron.Frontend.Route as R
import Neuron.Frontend.Route.Data.Types (SiteData, ZettelData (zettelDataPlugin))
import qualified Neuron.Frontend.Route.Data.Types as R
import Neuron.Frontend.Theme (Theme)
import qualified Neuron.Frontend.Theme as Theme
import Neuron.Frontend.Widget (elPreOverflowing, elTime, semanticIcon)
import Neuron.Markdown (ZettelParseError)
import qualified Neuron.Plugin as Plugin
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelC,
    ZettelT (..),
    sansContent,
  )
import qualified Neuron.Zettelkasten.Zettel as Z
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc.Raw (RawBuilder)
import Relude hiding ((&))
import Text.Pandoc.Definition (Block (Table), Pandoc, nullAttr)
import qualified Text.Pandoc.Walk as W

renderZettel ::
  ( DomBuilder t m,
    RawBuilder m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender t m
  ) =>
  SiteData ->
  ZettelData ->
  NeuronWebT t m ()
renderZettel siteData zData = do
  forM_ (DMap.toList $ zettelDataPlugin zData) $ \pluginData ->
    Plugin.renderPluginTop pluginData
  -- Main content
  elAttr "div" ("class" =: "ui text container" <> "id" =: "zettel-container" <> "style" =: "position: relative") $ do
    let elNeuronPandoc =
          divClass "pandoc"
            . Plugin.elZettel zData
            . addSemanticUIClasses
    divClass "zettel-view" $ do
      let zc = R.zettelDataZettel zData
          z = sansContent zc
      renderZettelContentCard elNeuronPandoc zc
      forM_ (DMap.toList $ zettelDataPlugin zData) $ \pluginData ->
        Plugin.renderPluginPanel elNeuronPandoc z pluginData
      renderBottomMenu
        (constDyn $ R.siteDataTheme siteData)
        (constDyn $ R.siteDataIndexZettel siteData)
        ((<> toText (zettelPath z)) <$> R.siteDataEditUrl siteData)

renderZettelContentCard ::
  (DomBuilder t m, PostBuild t m) =>
  (Pandoc -> NeuronWebT t m ()) ->
  ZettelC ->
  NeuronWebT t m ()
renderZettelContentCard elNeuronPandoc zc =
  case zc of
    Right z -> do
      renderZettelContent elNeuronPandoc z
    Left z -> do
      renderZettelRawContent z

renderBottomMenu ::
  (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) =>
  Dynamic t Theme ->
  -- | "Home" link
  Dynamic t (Maybe Zettel) ->
  -- | "Edit" URL for this route
  Maybe Text ->
  NeuronWebT t m ()
renderBottomMenu themeDyn mIndexZettel mEditUrl = do
  let attrs = ffor themeDyn $ \theme ->
        "class" =: ("ui bottom attached icon compact inverted menu " <> Theme.semanticColor theme)
          <> "id" =: "neuron-nav-bar"
  elDynAttr "nav" attrs $ do
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
      let linkAttrs = "href" =: editUrl <> "title" =: "Edit this page"
      elAttr "a" ("class" =: "item" <> linkAttrs) $ do
        semanticIcon "edit"
    -- Impulse
    R.neuronRouteLink (Some Route_Impulse) ("class" =: "right item" <> "title" =: "Open Impulse") $ do
      semanticIcon "wave square"

addSemanticUIClasses :: Pandoc -> Pandoc
addSemanticUIClasses = W.walk $ \case
  Table attrs@(ident, classes, kv) a b c d e
    | attrs == nullAttr ->
      -- Enable semantic UI table styling
      let classes' = classes <> ["ui", "table"]
       in Table (ident, classes', kv) a b c d e
  x ->
    x

renderZettelContent ::
  forall t m.
  (DomBuilder t m) =>
  (Pandoc -> NeuronWebT t m ()) ->
  ZettelT Pandoc ->
  NeuronWebT t m ()
renderZettelContent elNeuronPandoc Zettel {..} = do
  elClass "article" "ui raised attached segment zettel-content" $ do
    void $ elNeuronPandoc zettelContent
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
