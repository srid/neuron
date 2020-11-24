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

import Clay (Css, em, (?))
import qualified Clay as C
import Data.Foldable (maximum)
import qualified Data.Map.Strict as Map
import Data.TagTree (mkTagPattern)
import Data.Tree
import qualified Neuron.Web.Query.View as QueryView
import Neuron.Web.Route
import qualified Neuron.Web.Theme as Theme
import Neuron.Web.Widget (elPreOverflowing)
import Neuron.Web.Zettel.View (renderZettelParseError)
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (ZettelID (..), mkDefaultSlug)
import Neuron.Zettelkasten.Query (zettelsByTag)
import Neuron.Zettelkasten.Query.Error (showQueryResultError)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelError (..),
    ZettelT (zettelTitle),
  )
import Reflex.Dom.Core hiding (mapMaybe, (&))
import Relude hiding ((&))

-- | The value needed to render the z-index
--
-- All heavy graph computations are decoupled from rendering, producing this
-- value, that is in turn used for instant rendering.
data ZIndex = ZIndex
  { -- | Clusters on the folgezettel graph.
    zIndexClusters :: [Forest (Zettel, [Zettel])],
    zIndexOrphans :: [Zettel],
    -- | All zettel errors
    zIndexErrors :: Map ZettelID (NonEmpty ZettelError),
    zIndexStats :: Stats,
    zPinned :: [Zettel]
  }

data Stats = Stats
  { statsZettelCount :: Int,
    statsZettelConnectionCount :: Int
  }
  deriving (Eq, Show)

buildZIndex :: ZettelGraph -> Map ZettelID (NonEmpty ZettelError) -> ZIndex
buildZIndex graph errors =
  let (orphans, clusters) = partitionEithers $
        flip fmap (G.categoryClusters graph) $ \case
          [Node z []] -> Left z -- Orphans (cluster of exactly one)
          x -> Right x
      clustersWithBacklinks :: [Forest (Zettel, [Zettel])] =
        -- Compute backlinks for each node in the tree.
        flip fmap clusters $ \(zs :: [Tree Zettel]) ->
          G.backlinksMulti Folgezettel zs graph
      stats = Stats (length $ G.getZettels graph) (G.connectionCount graph)
      pinnedZettels = zettelsByTag (G.getZettels graph) [mkTagPattern "pinned"]
   in ZIndex (fmap sortCluster clustersWithBacklinks) orphans errors stats pinnedZettels
  where
    -- TODO: Either optimize or get rid of this (or normalize the sorting somehow)
    sortCluster fs =
      sortZettelForest $
        flip fmap fs $ \Node {..} ->
          Node rootLabel $ sortZettelForest subForest
    -- Sort zettel trees so that trees containing the most recent zettel (by ID) come first.
    sortZettelForest = reverse . sortOn maximum

renderZIndex ::
  DomBuilder t m =>
  Theme.Theme ->
  ZIndex ->
  NeuronWebT t m ()
renderZIndex (Theme.semanticColor -> themeColor) ZIndex {..} = do
  elClass "h1" "header" $ text "Zettel Index"
  renderErrors zIndexErrors
  divClass "z-index" $ do
    forM_ (nonEmpty zPinned) $ \zs ->
      divClass "ui pinned raised segment" $ do
        elClass "h3" "ui header" $ text "Pinned"
        el "ul" $
          forM_ zs $ \z ->
            el "li" $ QueryView.renderZettelLink Nothing Nothing def z
    whenNotNull zIndexOrphans $ \(toList -> zs) ->
      divClass ("ui piled segment") $ do
        elClass "p" "info" $ do
          text "Notes without any "
          elAttr "a" ("href" =: "https://neuron.zettel.page/linking.html") $ text "folgezettel"
          text " relationships"
        el "ul" $
          forM_ zs $ \z ->
            el "li" $ do
              QueryView.renderZettelLink Nothing Nothing def z
    forM_ zIndexClusters $ \forest ->
      divClass ("ui " <> themeColor <> " segment") $ do
        el "ul" $ renderForest forest
    el "p" $ do
      text $
        "The zettelkasten has "
          <> countNounBe "zettel" "zettels" (statsZettelCount zIndexStats)
          <> " and "
          <> countNounBe "link" "links" (statsZettelConnectionCount zIndexStats)
      text $ ". It has " <> countNounBe "cluster" "clusters" (length zIndexClusters) <> " in its folgezettel graph. "
      text "Each cluster's "
      elAttr "a" ("href" =: "https://neuron.zettel.page/folgezettel-heterarchy.html") $ text "folgezettel heterarchy"
      text " is rendered as a forest."
  where
    countNounBe noun nounPlural = \case
      1 -> "1 " <> noun
      n -> show n <> " " <> nounPlural

renderErrors :: DomBuilder t m => Map ZettelID (NonEmpty ZettelError) -> NeuronWebT t m ()
renderErrors errors = do
  let severity = \case
        ZettelError_ParseError _ -> "negative"
        ZettelError_QueryResultErrors _ -> "warning"
        ZettelError_AmbiguousFiles _ -> "negative"
        ZettelError_SlugConflict _ -> "negative"
      errorMessageHeader zid = \case
        ZettelError_ParseError _ -> do
          text "Zettel "
          QueryView.renderZettelLinkIDOnly zid (mkDefaultSlug $ unZettelID zid)
          text " failed to parse"
        ZettelError_QueryResultErrors (slug, _) -> do
          text "Zettel "
          QueryView.renderZettelLinkIDOnly zid slug
          text " has broken wiki-links"
        ZettelError_AmbiguousFiles _ -> do
          text $
            "More than one file define the same zettel ID ("
              -- TODO: not correct
              <> unZettelID zid
              <> "):"
        ZettelError_SlugConflict slug -> do
          text $ "Zettel " <> unZettelID zid <> " has slug " <> slug <> " used by other zettels"

  forM_ (Map.toList errors) $ \(zid, zErrors) ->
    forM_ zErrors $ \zError -> do
      divClass ("ui tiny message " <> severity zError) $ do
        divClass "header" $ errorMessageHeader zid zError
        el "p" $ do
          case zError of
            ZettelError_ParseError parseError ->
              renderZettelParseError parseError
            ZettelError_QueryResultErrors queryErrors ->
              el "ol" $ do
                forM_ (snd queryErrors) $ \qe ->
                  el "li" $ elPreOverflowing $ text $ showQueryResultError qe
            ZettelError_AmbiguousFiles filePaths ->
              el "ul" $ do
                forM_ filePaths $ \fp ->
                  el "li" $ el "tt" $ text $ toText fp
            ZettelError_SlugConflict _slug ->
              el "p" $ text "todo msg"

renderForest ::
  DomBuilder t m =>
  [Tree (Zettel, [Zettel])] ->
  NeuronWebT t m ()
renderForest trees = do
  forM_ trees $ \(Node (zettel, uplinks) subtrees) ->
    el "li" $ do
      QueryView.renderZettelLink Nothing Nothing def zettel
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
    "p.info" ? do
      C.color C.gray
    C.ul ? do
      C.listStyleType C.square
      C.paddingLeft $ em 1.5
    ".uplinks" ? do
      C.marginLeft $ em 0.3
