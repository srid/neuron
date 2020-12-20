module Impulse.Cache where

import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Language.Javascript.JSaddle
  ( FromJSVal (fromJSValUnchecked),
    MonadJSM,
    askJSM,
    jsg,
    runJSM,
  )
import Reflex.Dom

getCacheLocal ::
  forall a t m.
  ( MonadJSM m,
    FromJSON a,
    PostBuild t m,
    PerformEvent t m,
    MonadHold t m,
    TriggerEvent t m,
    MonadIO (Performable m)
  ) =>
  m (Dynamic t (Maybe (Either String a)))
getCacheLocal = do
  ctx <- askJSM
  v :: ByteString <- flip runJSM ctx $ do
    -- XXX: Figure out a way to read JSON directly, without parsing from string.
    -- That might make it perform better than getCacheRemote after all.
    cache <- jsg @Text "cacheText"
    s :: Text <- fromJSValUnchecked cache
    pure $ encodeUtf8 s
  pb <- getPostBuild
  -- XXX: do whole thing above inside performEventAsync?
  e <- performEventAsync $
    ffor pb $ \() fire ->
      liftIO $ fire $ Aeson.eitherDecodeStrict @a v
  holdDyn Nothing $ Just <$> e

getCacheRemote ::
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
getCacheRemote = do
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
        . traverse (Aeson.eitherDecode . BL.fromStrict . encodeUtf8)
        . _xhrResponse_responseText
