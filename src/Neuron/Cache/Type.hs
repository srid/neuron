{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Cache.Type where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Neuron.Config.Type (Config)
import Neuron.Frontend.Route.Data.Types (NeuronVersion)
import Neuron.Frontend.Widget (LoadableData (..))
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Zettel (shortRecordFields)
import Neuron.Zettelkasten.Zettel.Error (ZettelIssue)
import Reflex.Dom.Core
import Relude
import qualified Relude.String as BL

data ReadMode
  = ReadMode_Direct Config
  | ReadMode_Cached
  deriving (Eq, Show)

data NeuronCache = NeuronCache
  { neuroncacheGraph :: ZettelGraph,
    neuroncacheErrors :: Map ZettelID ZettelIssue,
    neuroncacheConfig :: Config,
    neuroncacheNeuronVersion :: NeuronVersion
  }
  deriving (Eq, Show, Generic)

instance ToJSON NeuronCache where
  toJSON = Aeson.genericToJSON shortRecordFields

instance FromJSON NeuronCache where
  parseJSON = Aeson.genericParseJSON shortRecordFields

reflexDomGetCache ::
  ( DomBuilder t m,
    Prerender js t m,
    TriggerEvent t m,
    PerformEvent t m,
    PostBuild t m,
    MonadHold t m
  ) =>
  LoadableData NeuronCache ->
  m (Dynamic t (LoadableData NeuronCache))
reflexDomGetCache staticCache = do
  join <$> prerender (pure $ constDyn staticCache) getCacheRemote
  where
    getCacheRemote = do
      -- TODO: refactor
      pb <- getPostBuild
      resp' <-
        performRequestAsyncWithError $
          XhrRequest "GET" "cache.json" def <$ pb
      let resp = ffor resp' $ first show >=> decodeXhrResponseWithError
      mresp <- fmap LoadableData <$> holdDyn Nothing (Just <$> resp)
      -- Workaround for thrice triggering bug?
      holdUniqDyn mresp
      where
        decodeXhrResponseWithError :: FromJSON a => XhrResponse -> Either String a
        decodeXhrResponseWithError =
          fromMaybe (Left "Empty response") . sequence
            . traverse (Aeson.eitherDecode . BL.fromStrict . encodeUtf8)
            . _xhrResponse_responseText
