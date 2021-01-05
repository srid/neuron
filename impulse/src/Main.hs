{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Impulse.Run as Run
import qualified Neuron.Cache.Type as C
import Neuron.Frontend.Route
  ( Route (Route_Impulse),
    routeConfig,
    runNeuronWeb,
  )
import qualified Neuron.Frontend.Route.Data as RD
import qualified Neuron.Frontend.View as V
import qualified Neuron.Frontend.Widget as W
import Reflex.Dom.Core (def)
import qualified Reflex.Dom.Main as Main

main :: IO ()
main =
  Run.run "cache.json" 3003 $ do
    let thisR = Route_Impulse Nothing
        w appendHead appendBody = do
          rec () <- appendHead $ do
                V.headTemplate thisR routeData
              routeData <- appendBody $ do
                cacheDyn <- C.reflexDomGetCache $ W.LoadableData Nothing
                let dataDyn = flip (fmap . fmap) cacheDyn $ \cache ->
                      (RD.mkSiteData cache def def, RD.mkImpulseData cache)
                runNeuronWeb routeConfig $ V.renderRouteImpulse dataDyn
                pure dataDyn
          pure ()
    Main.runHydrationWidgetWithHeadAndBody (pure ()) w