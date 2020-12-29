{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Impulse.Run as Run
import qualified Neuron.Web.Cache.Type as C
import Neuron.Web.Route
  ( Route (Route_Impulse),
    routeConfig,
    runNeuronWeb,
  )
import qualified Neuron.Web.Route.Data as RD
import qualified Neuron.Web.View as V
import qualified Reflex.Dom.Main as Main

main :: IO ()
main =
  Run.run "cache.json" 3003 $ do
    let thisR = Route_Impulse Nothing
        mkRD = \cache -> RD.mkRouteData mempty cache thisR
        fffmap :: (Functor f, Functor f1, Functor f2) => (a -> b) -> f (f1 (f2 a)) -> f (f1 (f2 b))
        fffmap = fmap . fmap . fmap
        w appendHead appendBody = do
          rec () <- appendHead $ do
                let dataDyn = (C._neuronCache_config &&& mkRD) `fffmap` cacheDyn
                V.headTemplate thisR dataDyn
              cacheDyn <- appendBody $ do
                c <- C.reflexDomGetCache Nothing
                let dataDyn = mkRD `fffmap` c
                runNeuronWeb routeConfig $ V.renderRouteImpulse dataDyn
                pure c
          pure ()
    Main.runHydrationWidgetWithHeadAndBody (pure ()) w