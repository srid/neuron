{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Impulse.Run as Run
import qualified Neuron.Web.Cache.Type as C
import Neuron.Web.Route
  ( Route (Route_Impulse),
    routeConfig,
    runNeuronWeb,
  )
import qualified Neuron.Web.View as V
import qualified Reflex.Dom.Main as Main

main :: IO ()
main =
  Run.run "cache.json" 3003 $ do
    let w appendHead appendBody = do
          cacheDyn <-
            appendHead $ do
              cacheDyn <- C.reflexDomGetCache Nothing
              let cfgDyn = fmap (fmap C._neuronCache_config) <$> cacheDyn
              V.headTemplate cfgDyn (Route_Impulse Nothing) ()
              pure cacheDyn
          appendBody $ do
            runNeuronWeb routeConfig $ V.renderRouteImpulse cacheDyn
    Main.runHydrationWidgetWithHeadAndBody (pure ()) w