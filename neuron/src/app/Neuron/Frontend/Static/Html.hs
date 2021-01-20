{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Static.Html where

import Control.Monad.Fix (MonadFix)
import Neuron.Frontend.Manifest (renderManifest)
import Neuron.Frontend.Route (Route (..))
import qualified Neuron.Frontend.Route as R
import qualified Neuron.Frontend.Route.Data.Types as R
import Neuron.Frontend.Static.HeadHtml (renderHeadHtml)
import Neuron.Frontend.Static.StructuredData (renderStructuredData)
import qualified Neuron.Frontend.View as V
import qualified Neuron.Frontend.Widget as W
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude
import qualified Skylighting.Core as Skylighting

-- | Render the given route
renderRoutePage ::
  forall t m js a.
  ( PandocBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender js t m,
    PerformEvent t m,
    TriggerEvent t m
  ) =>
  Route a ->
  Dynamic t (W.LoadableData a) ->
  m ()
renderRoutePage r val = do
  el "html" $ do
    el "head" $ do
      V.headTemplate r val
      W.loadingWidget' val blank (const blank) $ \valDyn ->
        dyn_ $
          ffor valDyn $ \v -> do
            renderHeadHtml $ R.siteDataHeadHtml (R.routeSiteData v r)
            renderManifest $ R.siteDataManifest (R.routeSiteData v r)
            renderStructuredData r v
      () <- case r of
        Route_Impulse ->
          blank
        Route_Zettel {} -> do
          elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
      pure ()
    el "body" $ do
      () <- case r of
        Route_Impulse {} -> do
          R.runNeuronWeb R.routeConfig $
            V.renderRouteImpulse val
        Route_Zettel {} -> do
          R.runNeuronWeb R.routeConfig $
            V.renderRouteZettel val
      pure ()
