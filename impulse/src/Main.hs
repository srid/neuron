{-# LANGUAGE RecordWildCards #-}
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
import qualified Reflex.Dom.Main as Main

main :: IO ()
main =
  Run.run "cache.json" 3003 $ do
    let thisR = Route_Impulse Nothing
        mkRD = \cache -> RD.mkRouteData mempty cache thisR
        w appendHead appendBody = do
          rec () <- appendHead $ do
                let dataDyn = fmap (C._neuronCache_config &&& mkRD) <$> cacheDyn
                V.headTemplate thisR dataDyn
              cacheDyn <- appendBody $ do
                c <- C.reflexDomGetCache $ W.LoadableData Nothing
                let dataDyn = fmap mkRD <$> c
                runNeuronWeb routeConfig $ V.renderRouteImpulse dataDyn
                pure c
          pure ()
    Main.runHydrationWidgetWithHeadAndBody (pure ()) w