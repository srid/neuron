{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Backend where

import Colog (WithLog, log)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import Neuron.CLI.Logging
import Relude

-- | Run a HTTP server to serve a directory of static files
--
-- Binds the server to host 127.0.0.1.
serve ::
  (MonadIO m, WithLog env Message m) =>
  -- | Host
  Text ->
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  FilePath ->
  m ()
serve host port path = do
  log (I' WWW) $ toText $ "Serving " <> path <> " at http://" <> toString host <> ":" <> show port
  liftIO $ Warp.runSettings settings app
  where
    app = staticApp $ defaultFileServerSettings path
    settings =
      Warp.setHost (fromString $ toString host) $
        Warp.setPort
          port
          Warp.defaultSettings