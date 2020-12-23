{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Widget where

import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Data.Time.DateMayTime (DateMayTime, formatDateMayTime, formatDay, getDay)
import Reflex.Dom.Core
import Relude

-- | <time> element
elTime :: DomBuilder t m => DateMayTime -> m ()
elTime t = do
  -- cf. https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time#Attributes
  elAttr "time" ("datetime" =: formatDateMayTime t) $ do
    text $ formatDay $ getDay t

-- | A pre element with scrollbar
elPreOverflowing :: DomBuilder t m => m a -> m a
elPreOverflowing = elAttr "pre" ("style" =: "overflow: auto")

semanticIcon :: DomBuilder t m => Text -> m ()
semanticIcon name = elClass "i" (name <> " icon") blank

elLinkGoogleFonts :: DomBuilder t m => [Text] -> m ()
elLinkGoogleFonts fs =
  let fsEncoded = T.intercalate "|" $ T.replace " " "+" <$> fs
      fsUrl = "https://fonts.googleapis.com/css?family=" <> fsEncoded <> "&display=swap"
   in elAttr "link" ("rel" =: "stylesheet" <> "href" =: fsUrl) blank

-- Prevent this element from appearing in Google search results
-- https://developers.google.com/search/reference/robots_meta_tag#data-nosnippet-attr
elNoSnippetSpan :: DomBuilder t m => Map Text Text -> m a -> m a
elNoSnippetSpan attrs = elAttr "span" ("data-nosnippet" =: "" <> attrs)

elVisible :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m a
elVisible visible w = do
  elDynAttr "span" (ffor visible $ bool ("style" =: "display: none;") mempty) w

divClassVisible :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Dynamic t Text -> m a -> m a
divClassVisible visible clsDyn w = do
  let attrs = ffor2 visible clsDyn $ \vis cls ->
        "class" =: cls <> bool ("style" =: "display: none;") mempty vis
  elDynAttr "div" attrs w

-- | Data that can be loaded.
--
-- Outter Maybe represents if the data is still loading (==Nothing).
-- Inner Either signals JSON decoding errors (==Left) if any.
type LoadableData a = Maybe (Either String a)

availableData :: a -> LoadableData a
availableData = Just . Right

loadingWidget ::
  (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) =>
  Dynamic t (LoadableData a) ->
  (Dynamic t a -> m ()) ->
  m ()
loadingWidget valDyn w = do
  mresp <- maybeDyn valDyn
  dyn_ $
    ffor mresp $ \case
      Nothing ->
        divClass "ui text container" inlineLoader
      Just resp -> do
        eresp <- eitherDyn resp
        dyn_ $
          ffor eresp $ \case
            Left errDyn -> do
              divClass "ui text container" $
                divClass "ui negative message" $ do
                  divClass "header" $ text "Unable to parse neuron cache"
                  el "p" $ dynText $ T.pack <$> errDyn
            Right aDyn -> do
              w aDyn
  where
    _loader :: DomBuilder t m => m ()
    _loader = do
      divClass "ui text container" $ do
        divClass "ui active dimmer" $ do
          divClass "ui medium text loader" $ text "Loading JSON cache"
        el "p" blank
    inlineLoader :: DomBuilder t m => m ()
    inlineLoader = do
      divClass "ui basic segment" $ do
        divClass "ui active centered inline loader" blank
