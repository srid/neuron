{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Impulse
  ( renderImpulse,
    buildImpulse,
    Impulse (..),
    style,
  )
where

import Clay (Css, em, (?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import Data.Foldable (maximum)
import qualified Data.Map.Strict as Map
import Data.TagTree (mkTagPattern, unTag)
import qualified Data.Text as T
import Data.Tree (Forest, Tree (..))
import qualified Neuron.Web.Query.View as QueryView
import Neuron.Web.Route (NeuronWebT)
import qualified Neuron.Web.Theme as Theme
import Neuron.Web.Widget (divClassVisible, elPreOverflowing, elVisible)
import Neuron.Web.Zettel.View (renderZettelParseError)
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (ZettelID (..))
import Neuron.Zettelkasten.Query (zettelsByTag)
import Neuron.Zettelkasten.Query.Error (showQueryResultError)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelError (..),
    ZettelT (zettelTitle),
    zettelTags,
  )
import Reflex.Dom.Core hiding (mapMaybe, (&))
import Relude hiding ((&))

-- | The value needed to render the z-index
--
-- All heavy graph computations are decoupled from rendering, producing this
-- value, that is in turn used for instant rendering.
data Impulse = Impulse
  { -- | Clusters on the folgezettel graph.
    impulseClusters :: [Forest (Zettel, [Zettel])],
    impulseOrphans :: [Zettel],
    -- | All zettel errors
    impulseErrors :: Map ZettelID ZettelError,
    impulseStats :: Stats,
    impulsePinned :: [Zettel]
  }

data Stats = Stats
  { statsZettelCount :: Int,
    statsZettelConnectionCount :: Int
  }
  deriving (Eq, Show)

-- TODO: Create SearchQuery.hs, and make a note of sharing it with CLI search.
data TreeMatch
  = -- | Tree's root matches the query.
    -- Subtrees may or may not match.
    TreeMatch_Root
  | -- | Tree's root does not match.
    -- However, one of the subtrees match.
    TreeMatch_Under
  deriving (Eq, Show)

searchTree :: (a -> Bool) -> Tree a -> Maybe (Tree (TreeMatch, a))
searchTree f (Node x children) = do
  let children' = catMaybes $ searchTree f <$> children
      tm
        | f x = Just TreeMatch_Root
        | null children' = Nothing
        | otherwise = Just TreeMatch_Under
  m <- tm
  pure $ Node (m, x) children'

buildImpulse :: ZettelGraph -> Map ZettelID ZettelError -> Impulse
buildImpulse graph errors =
  let (orphans, clusters) = partitionEithers $
        flip fmap (G.categoryClusters graph) $ \case
          [Node z []] -> Left z -- Orphans (cluster of exactly one)
          x -> Right x
      clustersWithUplinks :: [Forest (Zettel, [Zettel])] =
        -- Compute backlinks for each node in the tree.
        flip fmap clusters $ \(zs :: [Tree Zettel]) ->
          G.backlinksMulti Folgezettel zs graph
      stats = Stats (length $ G.getZettels graph) (G.connectionCount graph)
      pinnedZettels = zettelsByTag (G.getZettels graph) [mkTagPattern "pinned"]
   in Impulse (fmap sortCluster clustersWithUplinks) orphans errors stats pinnedZettels
  where
    -- TODO: Either optimize or get rid of this (or normalize the sorting somehow)
    sortCluster fs =
      sortZettelForest $
        flip fmap fs $ \Node {..} ->
          Node rootLabel $ sortZettelForest subForest
    -- Sort zettel trees so that trees containing the most recent zettel (by ID) come first.
    sortZettelForest = sortOn (Down . maximum)

renderImpulse ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  Theme.Theme ->
  Impulse ->
  -- | Search query to filter
  Dynamic t (Maybe Text) ->
  NeuronWebT t m ()
renderImpulse (Theme.semanticColor -> themeColor) Impulse {..} mqDyn = do
  elClass "h1" "header" $ do
    text "Impulse"
    dyn_ $
      ffor mqDyn $ \mq -> forM_ mq $ \q -> do
        text " ["
        el "tt" $ text q
        text "]"
  elVisible (ffor mqDyn $ \mq -> isNothing mq && not (null impulseErrors)) $
    elClass "details" "ui tiny errors message" $ do
      el "summary" $ text "Errors"
      renderErrors impulseErrors
  divClass "z-index" $ do
    let pinned = ffor mqDyn $ \mq -> filter (matchZettel mq) impulsePinned
    divClassVisible (not . null <$> pinned) "ui pinned raised segment" $ do
      elClass "h3" "ui header" $ text "Pinned"
      el "ul" $
        void $
          simpleList pinned $ \zDyn ->
            dyn_ $ ffor zDyn $ \z -> zettelLink z blank
    let orphans = ffor mqDyn $ \mq -> filter (matchZettel mq) impulseOrphans
    divClassVisible (not . null <$> orphans) "ui segment" $ do
      elClass "p" "info" $ do
        text "Notes without any "
        elAttr "a" ("href" =: "https://neuron.zettel.page/linking.html") $ text "folgezettel"
        text " relationships"
      el "ul" $
        void $
          simpleList orphans $ \zDyn ->
            dyn_ $ ffor zDyn $ \z -> zettelLink z blank
    let clusters = ffor mqDyn $ \mq ->
          ffor impulseClusters $ \forest ->
            fforMaybe forest $ \tree -> do
              searchTree (matchZettel mq . fst) tree
    void $
      simpleList clusters $ \forestDyn ->
        divClassVisible (not . null <$> forestDyn) ("ui " <> themeColor <> " segment") $ do
          el "ul" $ renderForest forestDyn
    el "p" $ do
      text $
        "The zettelkasten has "
          <> countNounBe "zettel" "zettels" (statsZettelCount impulseStats)
          <> " and "
          <> countNounBe "link" "links" (statsZettelConnectionCount impulseStats)
      text $ ". It has " <> countNounBe "cluster" "clusters" (length impulseClusters) <> " in its folgezettel graph. "
      text "Each cluster's "
      elAttr "a" ("href" =: "https://neuron.zettel.page/folgezettel-heterarchy.html") $ text "folgezettel heterarchy"
      text " is rendered as a forest."
  where
    countNounBe noun nounPlural = \case
      1 -> "1 " <> noun
      n -> show n <> " " <> nounPlural
    matchZettel :: Maybe Text -> Zettel -> Bool
    matchZettel mq z =
      isNothing $ do
        q <- mq
        -- HACK: We should "parse" the query text propertly into an ADT, the
        -- more complex the query will become. For now, just looking for "tag:???"
        if "tag:" `T.isPrefixOf` q
          then do
            let ztag = T.drop 4 q
            guard $ ztag `notElem` fmap unTag (zettelTags z)
          else guard $ not $ T.toLower q `T.isInfixOf` T.toLower (zettelTitle z)

renderErrors :: DomBuilder t m => Map ZettelID ZettelError -> NeuronWebT t m ()
renderErrors errors = do
  let severity = \case
        ZettelError_ParseError _ -> "negative"
        ZettelError_QueryResultErrors _ -> "warning"
        ZettelError_AmbiguousID _ -> "negative"
        ZettelError_AmbiguousSlug _ -> "negative"
      errorMessageHeader zid = \case
        ZettelError_ParseError (slug, _) -> do
          text "Zettel "
          QueryView.renderZettelLinkIDOnly zid slug
          text " failed to parse"
        ZettelError_QueryResultErrors (slug, _) -> do
          text "Zettel "
          QueryView.renderZettelLinkIDOnly zid slug
          text " has missing wiki-links"
        ZettelError_AmbiguousID _files -> do
          text $
            "More than one file define the same zettel ID ("
              <> unZettelID zid
              <> "):"
        ZettelError_AmbiguousSlug _slug -> do
          text $ "Zettel '" <> unZettelID zid <> "' ignored; has ambiguous slug"
  forM_ (Map.toList errors) $ \(zid, zError) ->
    divClass ("ui tiny message " <> severity zError) $ do
      divClass "header" $ errorMessageHeader zid zError
      el "p" $ do
        case zError of
          ZettelError_ParseError (_slug, parseError) ->
            renderZettelParseError parseError
          ZettelError_QueryResultErrors queryErrors ->
            el "ol" $ do
              forM_ (snd queryErrors) $ \qe ->
                el "li" $ elPreOverflowing $ text $ showQueryResultError qe
          ZettelError_AmbiguousID filePaths ->
            el "ul" $ do
              forM_ filePaths $ \fp ->
                el "li" $ el "tt" $ text $ toText fp
          ZettelError_AmbiguousSlug slug ->
            el "p" $ text $ "Slug '" <> slug <> "' is used by another zettel"

renderForest ::
  (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) =>
  Dynamic t [Tree (TreeMatch, (Zettel, [Zettel]))] ->
  NeuronWebT t m ()
renderForest treesDyn = do
  void $
    simpleList treesDyn $ \treeDyn -> do
      mDyn <- holdUniqDyn $ ffor treeDyn $ \(Node (m, _) _) -> m
      subtreesDyn <- holdUniqDyn $ ffor treeDyn $ \(Node _ subtrees) -> subtrees
      zup <- holdUniqDyn $ ffor treeDyn $ \(Node (_, x) _) -> x
      elDynClass "span" (ffor mDyn $ \m -> if m == TreeMatch_Root then "q root" else "q under") $ do
        dyn_ $
          ffor zup $ \(zettel, uplinks) -> do
            zettelLink zettel $ do
              when (length uplinks >= 2) $ do
                elClass "span" "uplinks" $ do
                  forM_ uplinks $ \z2 -> do
                    el "small" $
                      elAttr "i" ("class" =: "linkify icon" <> "title" =: zettelTitle z2) blank
        el "ul" $ renderForest subtreesDyn

zettelLink :: DomBuilder t m => Zettel -> NeuronWebT t m () -> NeuronWebT t m ()
zettelLink z w = do
  el "li" $ do
    QueryView.renderZettelLink Nothing Nothing def z
    w

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
  ".errors" ? do
    blank
  -- Display non-matching parents of matching nodes deemphasized
  ".q.under > li > span.zettel-link-container span.zettel-link a" ? do
    C.important $ C.color C.gray
