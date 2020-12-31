{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Backend where

import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import Relude

-- | Run a HTTP server to serve a directory of static files
--
-- Binds the server to host 127.0.0.1.
serve ::
  -- | Host
  Text ->
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  FilePath ->
  IO ()
serve host port path = do
  putStrLn $ "[www] Serving " <> path <> " at http://" <> toString host <> ":" <> show port
  Warp.runSettings settings app
  where
    app = staticApp $ defaultFileServerSettings path
    settings =
      Warp.setHost (fromString $ toString host) $
        Warp.setPort
          port
          Warp.defaultSettings