{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Some (Some, withSome)
import qualified Data.Text as T
import qualified Impulse.Run as Run
import Language.Javascript.JSaddle (MonadJSM)
import Neuron.Web.Cache.Type (NeuronCache)
import qualified Neuron.Web.Cache.Type as C
import qualified Neuron.Web.Impulse as Impulse
import Neuron.Web.Route
  ( NeuronWebT,
    Route (..),
    RouteConfig (RouteConfig),
    routeHtmlPath,
    runNeuronWeb,
  )
import qualified Neuron.Web.Theme as Theme
import qualified Neuron.Web.View as V
import Reflex.Dom.Core
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam)

main :: IO ()
main =
  Run.run "cache.json" 3003 $
    Run.mainWidgetWithHeadOnJsaddleOnly (V.headTemplate $ text "Impulse (dev)") bodyWidget

bodyWidget ::
  forall t m.
  ( DomBuilder t m,
    MonadFix m,
    PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    HasJSContext (Performable m),
    MonadJSM (Performable m),
    MonadHold t m,
    MonadJSM m
  ) =>
  m ()
bodyWidget = do
  mresp <- maybeDyn =<< getCache @C.NeuronCache
  dyn_ $
    ffor mresp $ \case
      Nothing -> loader
      Just resp -> do
        eresp <- eitherDyn resp
        dyn_ $
          ffor eresp $ \case
            Left errDyn -> do
              text "ERROR: "
              dynText $ T.pack <$> errDyn
            Right nDyn ->
              -- TODO: push dynamic inner?
              dyn_ $ ffor nDyn renderPage

loader :: DomBuilder t m => m ()
loader = do
  divClass "ui text container" $ do
    divClass "ui active dimmer" $ do
      divClass "ui medium text loader" $ text "Fetching JSON cache"
    el "p" blank

renderPage ::
  ( PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadJSM m,
    DomBuilder t m,
    MonadHold t m,
    MonadFix m,
    MonadIO (Performable m)
  ) =>
  NeuronCache ->
  m ()
renderPage C.NeuronCache {..} = do
  V.bodyTemplate _neuronCache_neuronVersion _neuronCache_config $ do
    divClass "ui text container" $ do
      mquery0 <- urlQueryVal [queryKey|q|]
      qDyn <- searchInput mquery0
      let impulse =
            Impulse.buildImpulse _neuronCache_graph _neuronCache_errors
      runNeuronGhcjs $ Impulse.renderImpulse Theme.Red impulse qDyn

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
    qSlow <- debounce 0.5 $ updated qDyn
    holdDyn mquery0 $ fmap (\q -> if q == "" then Nothing else Just q) qSlow

-- Return the value for given query key (eg: ?q=???) from the URL location.
urlQueryVal :: MonadJSM m => URI.RText 'URI.QueryKey -> m (Maybe Text)
urlQueryVal key = do
  uri <- URI.mkURI @Maybe <$> getLocationUrl
  pure $ getQueryParam key =<< uri

getCache ::
  forall a t m.
  ( PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    HasJSContext (Performable m),
    MonadJSM (Performable m),
    MonadHold t m,
    MonadFix m,
    FromJSON a,
    Eq a
  ) =>
  m (Dynamic t (Maybe (Either String a)))
getCache = do
  -- TODO: refactor
  pb <- getPostBuild
  resp' <-
    performRequestAsyncWithError $
      XhrRequest "GET" "cache.json" def <$ pb
  let resp = ffor resp' $ first show >=> decodeXhrResponseWithError
  mresp <- holdDyn Nothing $ Just <$> resp
  -- Workaround for thrice triggering bug?
  holdUniqDyn mresp
  where
    decodeXhrResponseWithError :: XhrResponse -> Either String a
    decodeXhrResponseWithError =
      fromMaybe (Left "Empty response") . sequence
        . traverse (eitherDecode . BL.fromStrict . encodeUtf8)
        . _xhrResponse_responseText

runNeuronGhcjs :: NeuronWebT t m a -> m a
runNeuronGhcjs = runNeuronWeb routeConfigGhcjs

routeConfigGhcjs :: RouteConfig t m
routeConfigGhcjs =
  RouteConfig False renderRouteLink someRouteUrl
  where
    renderRouteLink someR attrs =
      elAttr "a" (attrs <> "href" =: someRouteUrl someR)
    someRouteUrl :: Some Route -> Text
    someRouteUrl sr =
      toText $ withSome sr routeHtmlPath
