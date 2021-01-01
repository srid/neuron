{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Impulse
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
import Data.TagTree (mkDefaultTagQuery, mkTagPattern, unTag)
import qualified Data.Text as T
import Data.Tree (Forest, Tree (..))
import qualified Neuron.Frontend.Query.View as QueryView
import Neuron.Frontend.Route
import qualified Neuron.Frontend.Theme as Theme
import Neuron.Frontend.Widget (LoadableData, divClassVisible, elVisible)
import qualified Neuron.Frontend.Widget as W
import Neuron.Frontend.Zettel.View (renderZettelParseError)
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (ZettelID (..))
import Neuron.Zettelkasten.Query (zettelsByTag)
import qualified Neuron.Zettelkasten.Query.Error as Q
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelT (zettelTitle),
    zettelTags,
  )
import Neuron.Zettelkasten.Zettel.Error (ZettelError (..), ZettelIssue (..), splitZettelIssues)
import Reflex.Dom.Core
import Relude hiding ((&))
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam)

-- TODO: Create SearchQuery.hs, and make a note of sharing it with CLI search.
data TreeMatch
  = -- | Tree's root matches the query.
    -- Subtrees may or may not match.
    TreeMatch_Root
  | -- | Tree's root does not match.
    -- However, one of the subtrees match.
    TreeMatch_Under
  deriving (Eq, Show)

searchTree :: (a -> Bool) -> Tree a -> Tree (Maybe TreeMatch, a)
searchTree f (Node x xs) =
  let children = searchTree f <$> xs
      tm
        | f x = Just TreeMatch_Root
        | any treeMatches children = Just TreeMatch_Under
        | otherwise = Nothing
   in Node (tm, x) children

treeMatches :: Tree (Maybe a, b) -> Bool
treeMatches (Node (mm, _) _) = isJust mm

buildImpulse :: ZettelGraph -> Map ZettelID ZettelIssue -> Impulse
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
      pinnedZettels = zettelsByTag (G.getZettels graph) $ mkDefaultTagQuery [mkTagPattern "pinned"]
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
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) =>
  Dynamic t (Maybe Theme.Theme) ->
  Dynamic t (LoadableData Impulse) ->
  NeuronWebT t m ()
renderImpulse (fmap (fmap Theme.semanticColor) -> themeColor) impulseLDyn = do
  mqDyn <- fmap join $
    prerender (pure $ constDyn Nothing) $ do
      searchInput =<< urlQueryVal [queryKey|q|]
  elClass "h1" "header" $ do
    text "Impulse"
    el "span" $
      dyn_ $
        ffor mqDyn $ \mq -> forM_ mq $ \q -> do
          text " ["
          el "tt" $ text q
          text "]"
  -- Don't put static note in the static part of prerender; else it will appear on
  -- the static version as well, which is confusing.
  prerender_ blank staticVersionNote
  W.loadingWidget impulseLDyn $ \impulseDyn -> do
    elVisible (ffor2 (impulseErrors <$> impulseDyn) mqDyn $ \errs mq -> isNothing mq && not (null errs)) $
      elClass "details" "ui tiny errors message" $ do
        el "summary" $ text "Errors"
        renderErrors $ impulseErrors <$> impulseDyn
    divClass "z-index" $ do
      pinned <- holdUniqDyn $ ffor2 (impulsePinned <$> impulseDyn) mqDyn $ \v mq -> filter (matchZettel mq) v
      divClassVisible (not . null <$> pinned) "ui pinned raised segment" $ do
        elClass "h3" "ui header" $ text "Pinned"
        el "ul" $
          void $
            simpleList pinned $ \zDyn ->
              dyn_ $ ffor zDyn $ \z -> zettelLink z blank
      orphans <- holdUniqDyn $ ffor2 (impulseOrphans <$> impulseDyn) mqDyn $ \v mq -> filter (matchZettel mq) v
      divClassVisible (not . null <$> orphans) "ui segment" $ do
        elClass "p" "info" $ do
          text "Notes without any "
          elAttr "a" ("href" =: "https://neuron.zettel.page/linking.html") $ text "folgezettel"
          text " relationships"
        el "ul" $
          void $
            simpleList orphans $ \zDyn ->
              dyn_ $ ffor zDyn $ \z -> zettelLink z blank
      clusters <- holdUniqDyn $
        ffor2 (impulseClusters <$> impulseDyn) mqDyn $ \cs mq ->
          ffor cs $ \forest ->
            ffor forest $ \tree -> do
              searchTree (matchZettel mq . fst) tree
      void $
        simpleList clusters $ \forestDyn -> do
          let visible = any treeMatches <$> forestDyn
          divClassVisible visible ("ui " <> (fromMaybe "" <$> themeColor) <> " segment") $ do
            el "ul" $ renderForest forestDyn
      el "p" $ do
        let stats = impulseStats <$> impulseDyn
        text "The zettelkasten has "
        dynText $ countNounBe "zettel" "zettels" . statsZettelCount <$> stats
        text " and "
        dynText $ countNounBe "link" "links" . statsZettelConnectionCount <$> stats
        text ". It has "
        dynText $ countNounBe "cluster" "clusters" . length . impulseClusters <$> impulseDyn
        text " in its folgezettel graph. "
        text "Each cluster's "
        elAttr "a" ("href" =: "https://neuron.zettel.page/folgezettel-heterarchy.html") $ text "folgezettel heterarchy"
        text " is rendered as a forest."
  where
    -- Return the value for given query key (eg: ?q=???) from the URL location.
    -- urlQueryVal :: MonadJSM m => URI.RText 'URI.QueryKey -> m (Maybe Text)
    urlQueryVal key = do
      uri <- URI.mkURI @Maybe <$> getLocationUrl
      pure $ getQueryParam key =<< uri
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
    staticVersionNote = do
      el "p" $ do
        text "A static version of this page is available "
        elAttr "a" ("href" =: toText (routeHtmlPath Route_ImpulseStatic)) $ text "here"
        text "."

renderErrors ::
  (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) =>
  Dynamic t (Map ZettelID ZettelIssue) ->
  NeuronWebT t m ()
renderErrors issues = do
  let (errors, missing) = fmap fst &&& fmap snd $ fmap splitZettelIssues issues
  void $
    simpleList errors $ \errorDyn -> do
      dyn_ $
        ffor errorDyn $ \(zid, err) -> do
          renderError zid err
  mzs404 <- maybeDyn $ nonEmpty <$> missing
  dyn_ $
    ffor mzs404 $
      flip whenJust $ \zs404 -> do
        divClass "ui tiny message warning" $ do
          divClass "header" $ text "Missing wiki-links detected in some zettels"
          el "p" $ do
            divClass "ui horizontal list" $ do
              void $
                simpleList (toList <$> zs404) $ \zDyn ->
                  divClass "item" $ do
                    dyn_ $
                      ffor zDyn $ \(zid, (slug, qErrors)) -> do
                        let tooltip =
                              "Links in vain to: "
                                <> T.intercalate ", " (toList $ unZettelID <$> Q.missingZids qErrors)
                        elAttr "span" ("title" =: tooltip) $ do
                          QueryView.renderZettelLinkIDOnly zid slug
  where
    renderError zid zError = do
      case zError of
        ZettelError_ParseError (_slug, parseError) -> errorDiv zid zError $ do
          renderZettelParseError parseError
        ZettelError_AmbiguousID filePaths -> errorDiv zid zError $ do
          el "ul" $ do
            forM_ filePaths $ \fp ->
              el "li" $ el "tt" $ text $ toText fp
        ZettelError_AmbiguousSlug slug -> errorDiv zid zError $ do
          el "p" $ text $ "Slug '" <> slug <> "' is used by another zettel"
    errorDiv zid zError w = do
      divClass ("ui tiny message " <> severity zError) $ do
        divClass "header" $ errorMessageHeader zid zError
        el "p" w
    severity = \case
      ZettelError_ParseError _ -> "negative"
      ZettelError_AmbiguousID _ -> "negative"
      ZettelError_AmbiguousSlug _ -> "negative"
    errorMessageHeader zid = \case
      ZettelError_ParseError (slug, _) -> do
        text "Zettel "
        QueryView.renderZettelLinkIDOnly zid slug
        text " failed to parse"
      ZettelError_AmbiguousID _files -> do
        text $
          "More than one path is associated with the same zettel ID ("
            <> unZettelID zid
            <> "):"
      ZettelError_AmbiguousSlug _slug -> do
        text $ "Zettel '" <> unZettelID zid <> "' ignored; has ambiguous slug"

renderForest ::
  (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) =>
  Dynamic t [Tree (Maybe TreeMatch, (Zettel, [Zettel]))] ->
  NeuronWebT t m ()
renderForest treesDyn = do
  void $
    simpleList treesDyn $ \treeDyn -> do
      mDyn <- holdUniqDyn $ ffor treeDyn $ \(Node (m, _) _) -> m
      subtreesDyn <- holdUniqDyn $ ffor treeDyn $ \(Node _ subtrees) -> subtrees
      zup <- holdUniqDyn $ ffor treeDyn $ \(Node (_, x) _) -> x
      let treeClass = ffor mDyn $ \case
            Just TreeMatch_Root -> "q root"
            Just TreeMatch_Under -> "q under"
            Nothing -> "q unmatched"
      elDynClass "span" treeClass $ do
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

searchInput ::
  ( DomBuilder t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadFix m
  ) =>
  Maybe Text ->
  m (Dynamic t (Maybe Text))
searchInput mquery0 = do
  divClass "ui fluid icon input search" $ do
    qDyn <-
      fmap value $
        inputElement $
          def
            & initialAttributes
              .~ ("placeholder" =: "Search here ..." <> "autofocus" =: "")
            & inputElementConfig_initialValue .~ fromMaybe "" mquery0
    elClass "i" "search icon fas fa-search" blank
    qSlow <- debounce 0.3 $ updated qDyn
    holdDyn mquery0 $ fmap (\q -> if q == "" then Nothing else Just q) qSlow

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
  ".q.unmatched" ? do
    C.display C.none
