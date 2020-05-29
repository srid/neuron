{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.ZIndex
  ( renderZIndex,
  )
where

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
import Neuron.Zettelkasten.Query.Error (QueryError, showQueryError)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Relude hiding ((&))

renderZIndex :: DomBuilder t m => Theme.Theme -> ZettelGraph -> Map ZettelID (Either Text [QueryError]) -> NeuronWebT t m ()
renderZIndex neuronTheme graph errors = do
  elClass "h1" "header" $ text "Zettel Index"
  renderErrors errors
  divClass "z-index" $ do
    -- Cycle detection.
    case G.topSort graph of
      Left (toList -> cyc) -> divClass "ui orange segment" $ do
        el "h2" $ text "Cycle detected"
        forM_ cyc $ \zettel ->
          el "li" $ QueryView.renderZettelLink Nothing def zettel
      _ -> blank
    let clusters = G.categoryClusters graph
    el "p" $ do
      text $ "There " <> countNounBe "cluster" "clusters" (length clusters) <> " in the Zettelkasten folgezettel graph. "
      text "Each cluster is rendered as a forest."
    forM_ clusters $ \forest ->
      divClass ("ui " <> Theme.semanticColor neuronTheme <> " segment") $ do
        -- Forest of zettels, beginning with mother vertices.
        el "ul" $ renderForest True Nothing (Just graph) forest
  el "br" blank
  where
    countNounBe noun nounPlural = \case
      1 -> "is 1 " <> noun
      n -> "are " <> show n <> " " <> nounPlural

renderErrors :: DomBuilder t m => Map ZettelID (Either Text [QueryError]) -> m ()
renderErrors errors = do
  let skippedZettels = Map.mapMaybe leftToMaybe errors
      zettelsWithErrors = Map.mapMaybe rightToMaybe errors
  unless (null skippedZettels) $ do
    divClass "ui small negative message" $ do
      divClass "header" $ do
        text "These files are excluded from the zettelkasten due to parse errors"
      el "p" $ do
        el "ol" $ do
          forM_ (Map.toList skippedZettels) $ \(zid, err) ->
            el "li" $ do
              el "b" $ el "tt" $ text $ toText $ zettelIDSourceFileName zid
              text ": "
              el "pre" $ text err
  forM_ (Map.toList zettelsWithErrors) $ \(zid, qerrors) ->
    divClass "ui tiny warning message" $ do
      divClass "header" $ do
        text $ "Zettel "
        elClass "span" "zettel-link-container" $ do
          elClass "span" "zettel-link" $ do
            elAttr "a" ("href" =: QueryView.zettelUrl zid) $ text $ zettelIDText zid
        text " has errors"
      el "p" $ do
        el "ol" $ do
          forM_ qerrors $ \qe ->
            el "li" $ el "pre" $ text $ showQueryError qe

renderForest ::
  DomBuilder t m =>
  Bool ->
  Maybe Int ->
  -- When given the zettelkasten graph, also show non-parent backlinks.
  -- The dfsForest tree is "incomplete" in that it lacks these references.
  Maybe ZettelGraph ->
  [Tree Zettel] ->
  NeuronWebT t m ()
renderForest isRoot maxLevel mg trees =
  case maxLevel of
    Just 0 -> blank
    _ -> do
      forM_ (sortForest trees) $ \(Node zettel subtrees) ->
        el "li" $ do
          let zettelDiv =
                divClass
                  (maybe "" (const "ui ") mg)
          bool id zettelDiv isRoot $
            QueryView.renderZettelLink Nothing def zettel
          whenJust mg $ \g -> do
            text " "
            case G.backlinks Folgezettel zettel g of
              conns@(_ : _ : _) ->
                -- Has two or more category backlinks
                forM_ conns $ \zettel2 -> do
                  let connTitle = (zettelIDText (zettelID zettel2) <> " " <> zettelTitle zettel2)
                  elAttr "i" ("class" =: "fas fa-link" <> "title" =: connTitle) blank
              _ -> blank
          when (length subtrees > 0) $ do
            el "ul" $ renderForest False ((\n -> n - 1) <$> maxLevel) mg subtrees
  where
    -- Sort trees so that trees containing the most recent zettel (by ID) come first.
    sortForest = reverse . sortOn maximum
