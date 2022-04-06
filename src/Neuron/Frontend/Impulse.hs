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
    style,
  )
where

import Clay (Css, em, (?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import qualified Data.Set as Set
import Data.TagTree (Tag (..))
import Data.Tagged
import qualified Data.Text as T
import Data.Tree (Tree (..))
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Frontend.Route.Data.Types
import qualified Neuron.Frontend.Theme as Theme
import Neuron.Frontend.Widget (LoadableData, divClassVisible, elVisible)
import qualified Neuron.Frontend.Widget as W
import Neuron.Frontend.Zettel.View (renderZettelParseError)
import qualified Neuron.Frontend.Zettel.View as ZettelView
import qualified Neuron.Plugin.Plugins.Links as Links
import qualified Neuron.Plugin.Plugins.Tags as Tags
import Neuron.Zettelkasten.ID (ZettelID (..))
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelT (zettelTitle),
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

renderImpulse ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) =>
  Dynamic t (LoadableData (SiteData, ImpulseData)) ->
  NeuronWebT t m ()
renderImpulse dataLDyn = do
  -- themeDyn indexZettel impulseLDyn = do
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
  W.loadingWidget dataLDyn $ \dataDyn -> do
    let impulseDyn = snd <$> dataDyn
        themeDyn = siteDataTheme . fst <$> dataDyn
        indexZettel = siteDataIndexZettel . fst <$> dataDyn
    elVisible (ffor2 (impulseDataErrors <$> impulseDyn) mqDyn $ \errs mq -> isNothing mq && not (null errs)) $
      elClass "details" "ui tiny errors message" $ do
        el "summary" $ text "Errors"
        renderErrors $ impulseDataErrors <$> impulseDyn
    divClass "z-index" $ do
      pinned <- holdUniqDyn $ ffor2 (impulseDataPinned <$> impulseDyn) mqDyn $ \v mq -> filter (matchZettel mq) v
      divClassVisible (not . null <$> pinned) "ui pinned raised segment" $ do
        elClass "h3" "ui header" $ text "Pinned"
        el "ul" $
          void $
            simpleList pinned $ \zDyn ->
              dyn_ $ ffor zDyn $ \z -> zettelLink z blank
      orphans <- holdUniqDyn $ ffor2 (impulseDataOrphans <$> impulseDyn) mqDyn $ \v mq -> filter (matchZettel mq) v
      divClassVisible (not . null <$> orphans) "ui segment" $ do
        elClass "p" "info" $ do
          text "Notes not belonging to any "
          elAttr "a" ("href" =: "https://neuron.zettel.page/folgezettel-heterarchy") $ text "heterarchy"
          text ":"
        el "ul" $
          void $
            simpleList orphans $ \zDyn ->
              dyn_ $ ffor zDyn $ \z -> zettelLink z blank
      clusters <- holdUniqDyn $
        ffor2 (impulseDataClusters <$> impulseDyn) mqDyn $ \cs mq ->
          ffor cs $ \forest ->
            ffor forest $ \tree -> do
              searchTree (matchZettel mq . fst) tree
      void $
        simpleList clusters $ \forestDyn -> do
          let visible = any treeMatches <$> forestDyn
          divClassVisible visible ("ui " <> (Theme.semanticColor <$> themeDyn) <> " segment") $ do
            el "ul" $ renderForest forestDyn
      divClass "ui top attached segment" $ do
        el "p" $ do
          let stats = impulseDataStats <$> impulseDyn
          text "The notebook has "
          dynText $ countNounBe "note" "notes" . statsZettelCount <$> stats
          text " and "
          dynText $ countNounBe "link" "links" . statsZettelConnectionCount <$> stats
          text ". It has "
          dynText $ countNounBe "cluster" "clusters" . length . impulseDataClusters <$> impulseDyn
          text " in its folgezettel graph. "
          text "Each cluster's "
          elAttr "a" ("href" =: "https://neuron.zettel.page/folgezettel-heterarchy") $ text "folgezettel heterarchy"
          text " is rendered as a forest."
      -- TODO: Use dynamic throughout instead of defaulting the theme
      ZettelView.renderBottomMenu themeDyn indexZettel Nothing
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
        guard $
          not $
            if "tag:" `T.isPrefixOf` q
              then
                let ztag = T.drop 4 q
                 in Tag ztag `Set.member` Tags.getZettelTags z
              else T.toLower q `T.isInfixOf` T.toLower (zettelTitle z)

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
                      ffor zDyn $ \(zid, (slug, missingZids)) -> do
                        let tooltip =
                              "Links in vain to: "
                                <> T.intercalate ", " (toList $ unZettelID . untag <$> missingZids)
                        elAttr "span" ("title" =: tooltip) $ do
                          Links.renderZettelLinkIDOnly zid slug
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
        Links.renderZettelLinkIDOnly zid slug
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

zettelLink :: (DomBuilder t m, PostBuild t m) => Zettel -> NeuronWebT t m () -> NeuronWebT t m ()
zettelLink z w = do
  el "li" $ do
    Links.renderZettelLink Nothing Nothing def z
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

-- Display non-matching parents of matching nodes deemphasized

-- ".q.under > li > span.zettel-link-container span.zettel-link a" ? do
--  C.important $ C.color C.gray
-- ".q.unmatched" ? do
--  C.display C.none
