{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- WIP Fork of Language.Javascript.JSaddle.WebSockets

-----------------------------------------------------------------------------

-- |
module Impulse.Run
  ( run,
    mainWidgetWithHeadOnJsaddleOnly,
  )
where

#ifndef ghcjs_HOST_OS
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
    (jsaddleJs,  jsaddleAppWithJsOr, jsaddleOr )
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
#else
import GHCJS.DOM.Types (JSM)
#endif

import Reflex.Dom (Widget)
import qualified Reflex.Dom.Main as Main

-- | Run the given 'JSM' action as the main entry point.  Either directly
--   in GHCJS or as a Warp server on the given port on GHC.
#ifdef ghcjs_HOST_OS
run :: FilePath -> Int -> IO () -> IO ()
run _dataPath _port = id
#else
run :: FilePath -> Int -> JSM () -> IO ()
run dataPath port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleAppOr
    where
      jsaddleAppOr =
        jsaddleAppWithJsOr (jsaddleJs False) otherApp
      otherApp req sendResponse = do
        resp <- case W.rawPathInfo req of
          "/cache.json" -> do
            s <- readFileLBS dataPath
            pure $ W.responseLBS H.status200 [("Content-Type", "text/plain")] s
          _path ->
            pure $ W.responseLBS H.status403 [("Content-Type", "text/plain")] "Forbidden"
        sendResponse resp
#endif

-- | Like `mainWidgetWithHead` but the head widget is injected on GHC (jsaddle)
-- only; and not GHCJS.
--
-- This is useful if the head widget is statically generated (using dynamic
-- configuration which wouldn't be available in `mainWidgetWithHead`) on the
-- HTML on which this JS is evaluated, but we still want to inject the some head
-- with the same JS/CSS assets explicitly during GHC development.
#ifdef ghcjs_HOST_OS
{-# INLINABLE mainWidgetWithHeadOnJsaddleOnly #-}
mainWidgetWithHeadOnJsaddleOnly :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> JSM ()
mainWidgetWithHeadOnJsaddleOnly _headWidget bodyWidget = do
  Main.mainWidget bodyWidget
#else
{-# INLINABLE mainWidgetWithHeadOnJsaddleOnly #-}
mainWidgetWithHeadOnJsaddleOnly :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> JSM ()
mainWidgetWithHeadOnJsaddleOnly headWidget bodyWidget = do
  Main.mainWidgetWithHead headWidget bodyWidget
#endif
