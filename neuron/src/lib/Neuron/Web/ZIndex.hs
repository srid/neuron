{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.ZIndex
  ( renderZIndex,
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

renderZIndex ::
  DomBuilder t m =>
  Theme.Theme ->
  ZettelGraph ->
  Map ZettelID ZettelError ->
  NeuronWebT t m ()
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
      text "Each cluster's "
      elAttr "a" ("href" =: "https://neuron.zettel.page/2017401.html") $ text "folgezettel heterarchy"
      text " is rendered as a forest."
    forM_ clusters $ \forest ->
      divClass ("ui " <> Theme.semanticColor neuronTheme <> " segment") $ do
        -- Forest of zettels, beginning with mother vertices.
        el "ul" $ renderForest True Nothing (Just graph) forest
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
                  el "small" $ elAttr "i" ("class" =: "linkify icon" <> "title" =: connTitle) blank
              _ -> blank
          when (length subtrees > 0) $ do
            el "ul" $ renderForest False ((\n -> n - 1) <$> maxLevel) mg subtrees
  where
    -- Sort trees so that trees containing the most recent zettel (by ID) come first.
    sortForest = reverse . sortOn maximum

style :: Css
style = do
  "div.z-index" ? do
    C.ul ? do
      C.listStyleType C.square
      C.paddingLeft $ em 1.5
