{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.ZIndex
  ( renderZIndex,
    buildZIndex,
    ZIndex (..),
    style,
  )
where

import Clay ((?), Css, em)
import qualified Clay as C
import Data.Foldable (maximum)
import qualified Data.Map.Strict as Map
import Data.Tree
import qualified Neuron.Web.Query.View as QueryView
import Neuron.Web.Route
import qualified Neuron.Web.Theme as Theme
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error (showQueryError)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Relude hiding ((&))

-- | The value needed to render the z-index
--
-- All heavy graph computations are decoupled from rendering, producing this
-- value, that is in turn used for instant rendering.
data ZIndex = ZIndex
  { -- | Topological ordering of the folgezettel graph. Left value indicates
    --  that it is cyclic.
    zIndexTopSort :: Either (NonEmpty Zettel) [Zettel],
    -- | Clusters on the folgezettel graph.
    zIndexClusters :: [Forest (Zettel, [Zettel])],
    -- | All zettel errors
    zIndexErrors :: Map ZettelID ZettelError
  }

buildZIndex :: ZettelGraph -> Map ZettelID ZettelError -> ZIndex
buildZIndex graph errors =
  -- TODO: Also buld backlinks of each res
  let clusters = G.categoryClusters graph
      clusters' :: [Forest (Zettel, [Zettel])] =
        flip fmap clusters $ \(zs :: [Tree Zettel]) ->
          G.backlinksMulti Folgezettel zs graph
      topSort = G.topSort graph
   in ZIndex topSort (fmap sortForest clusters') errors
  where
    -- TODO: Either optimize or get rid of this (or normalize the sorting somehow)
    sortForest fs =
      sortZettelForest $ flip fmap fs $ \Node {..} ->
        Node rootLabel $ sortZettelForest subForest
    -- Sort zettel trees so that trees containing the most recent zettel (by ID) come first.
    sortZettelForest = reverse . sortOn maximum

renderZIndex ::
  DomBuilder t m =>
  Theme.Theme ->
  ZIndex ->
  NeuronWebT t m ()
renderZIndex neuronTheme ZIndex {..} = do
  elClass "h1" "header" $ text "Zettel Index"
  renderErrors zIndexErrors
  divClass "z-index" $ do
    -- Cycle detection.
    case zIndexTopSort of
      Left (toList -> cyc) -> divClass "ui orange segment" $ do
        el "h2" $ text "Cycle detected"
        forM_ cyc $ \zettel ->
          el "li" $ QueryView.renderZettelLink Nothing def zettel
      _ -> blank
    el "p" $ do
      text $ "There " <> countNounBe "cluster" "clusters" (length zIndexClusters) <> " in the Zettelkasten folgezettel graph. "
      text "Each cluster's "
      elAttr "a" ("href" =: "https://neuron.zettel.page/2017401.html") $ text "folgezettel heterarchy"
      text " is rendered as a forest."
    forM_ zIndexClusters $ \forest ->
      divClass ("ui " <> Theme.semanticColor neuronTheme <> " segment") $ do
        -- Forest of zettels, beginning with mother vertices.
        el "ul" $ renderForest forest
  where
    countNounBe noun nounPlural = \case
      1 -> "is 1 " <> noun
      n -> "are " <> show n <> " " <> nounPlural

renderErrors :: DomBuilder t m => Map ZettelID ZettelError -> NeuronWebT t m ()
renderErrors errors = do
  let eitherError f1 f2 = either (const f1) (const f2)
  forM_ (Map.toList errors) $ \(zid, eError) ->
    divClass ("ui tiny message " <> eitherError "negative" "warning" eError) $ do
      divClass "header" $ do
        text $ "Zettel "
        QueryView.renderZettelLinkIDOnly zid
        text $ eitherError " failed to parse" " has malformed queries" eError
      el "p" $ do
        case eError of
          Left parseError ->
            el "pre" $ text $ show parseError
          Right queryErrors ->
            el "ol" $ do
              forM_ queryErrors $ \qe ->
                el "li" $ el "pre" $ text $ showQueryError qe

renderForest ::
  DomBuilder t m =>
  [Tree (Zettel, [Zettel])] ->
  NeuronWebT t m ()
renderForest trees = do
  forM_ trees $ \(Node (zettel, uplinks) subtrees) ->
    el "li" $ do
      QueryView.renderZettelLink Nothing def zettel
      when (length uplinks >= 2) $ do
        elClass "span" "uplinks" $ do
          forM_ uplinks $ \z2 -> do
            el "small" $
              elAttr "i" ("class" =: "linkify icon" <> "title" =: zettelTitle z2) blank
      unless (null subtrees) $ do
        el "ul" $ renderForest subtrees

style :: Css
style = do
  "div.z-index" ? do
    C.ul ? do
      C.listStyleType C.square
      C.paddingLeft $ em 1.5
    ".uplinks" ? do
      C.marginLeft $ em 0.3
