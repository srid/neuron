{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Rename to Link View or something. Or move to Links plugin?
module Neuron.Frontend.Query.View
  ( renderZettelLink,
    renderZettelLinkIDOnly,
    renderMissingZettelLink,
  )
where

import Data.Some (Some (..))
import Data.Tagged (untag)
import qualified Data.Text as T
import Neuron.Frontend.Route
  ( NeuronWebT,
    Route (..),
    neuronRouteLink,
  )
import Neuron.Frontend.Widget (elNoSnippetSpan, elTime)
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.ID (Slug, ZettelID (), unZettelID)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding (count, tag)
import Relude

-- | Render a link to an individual zettel.
renderZettelLink ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Link inner text
  Maybe (m ()) ->
  -- | Connection type to display
  Maybe Connection ->
  -- | Link theme
  Maybe LinkView ->
  Zettel ->
  NeuronWebT t m ()
renderZettelLink mInner conn (fromMaybe def -> linkView) Zettel {..} = do
  let connClass = show <$> conn
      rawClass = const (Just "errors") =<< untag zettelContent
      mextra =
        case linkView of
          LinkView_Default ->
            Nothing
          LinkView_ShowDate ->
            elTime <$> zettelDate
          LinkView_ShowID ->
            Just $ el "tt" $ text $ unZettelID zettelID
      classes :: [Text] = catMaybes $ [Just "zettel-link-container"] <> [connClass, rawClass]
  elClass "span" (T.intercalate " " classes) $ do
    forM_ mextra $ \extra ->
      elNoSnippetSpan ("class" =: "extra monoFont") $ do
        extra
        -- The extra space is so that double clicking on this extra text
        -- doesn't select the title next.
        text " "
    elAttr "span" ("class" =: "zettel-link" <> maybe mempty ("title" =:) linkTooltip) $ do
      let linkInnerHtml = fromMaybe (text zettelTitle) mInner
      neuronRouteLink (Some $ Route_Zettel zettelSlug) mempty linkInnerHtml
      elConnSuffix conn
  where
    -- If there is custom inner text, put zettel title in tooltip.
    linkTooltip
      | isJust mInner = Just $ "Zettel: " <> zettelTitle
      | otherwise = Nothing

elConnSuffix :: DomBuilder t m => Maybe Connection -> m ()
elConnSuffix mconn =
  case mconn of
    Just Folgezettel -> elNoSnippetSpan mempty $ do
      elAttr "sup" ("title" =: "Branching link (folgezettel)") $ text "ᛦ"
    _ -> pure mempty

-- TODO: Eventually refactor this function to reuse what's in renderZettelLink
renderMissingZettelLink :: DomBuilder t m => ZettelID -> m ()
renderMissingZettelLink zid = do
  let classes = ["zettel-link-container", "errors"]
  elClass "span" (T.intercalate " " classes) $ do
    let errMsg = "Wiki-link does not refer to any existing zettel"
    elAttr "span" ("class" =: "zettel-link" <> "title" =: errMsg) $ do
      elAttr "a" mempty $ text $ unZettelID zid

-- | Like `renderZettelLink` but when we only have ID in hand.
renderZettelLinkIDOnly :: (DomBuilder t m, PostBuild t m) => ZettelID -> Slug -> NeuronWebT t m ()
renderZettelLinkIDOnly zid slug =
  elClass "span" "zettel-link-container" $ do
    elClass "span" "zettel-link" $ do
      neuronRouteLink (Some $ Route_Zettel slug) mempty $ text $ unZettelID zid
