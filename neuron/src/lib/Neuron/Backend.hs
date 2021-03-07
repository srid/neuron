{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Backend where

import Colog (WithLog, log)
import qualified Data.Text as T
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Rewrite (rewriteWithQueries)
import Neuron.CLI.Logging
import Relude
import System.Directory (doesPathExist)
import System.FilePath ((</>))

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
    app =
      allowPrettyUrls $
        staticApp $
          defaultFileServerSettings path
    settings =
      Warp.setHost (fromString $ toString host) $
        Warp.setPort
          port
          Warp.defaultSettings
    allowPrettyUrls =
      rewriteWithQueries $ \pq _reqH -> do
        -- Serve /foo.html when asked for /foo, but only if foo.html exists on
        -- disk.
        case pq of
          ([oldPath@(T.stripSuffix ".html" -> Nothing)], []) -> do
            let newPath = toString $ oldPath <> ".html"
            doesPathExist (path </> newPath) >>= \case
              True -> pure ([toText newPath], [])
              False -> pure pq
          _ ->
            pure pq
