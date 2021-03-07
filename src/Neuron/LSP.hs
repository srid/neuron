{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Just a stub for now.
-- See https://github.com/srid/neuron/issues/213
module Neuron.LSP where

import Language.LSP.Server
import Language.LSP.Types
import Neuron.CLI.Types (App, MonadApp (getNotesDir))
import Relude

handlers :: FilePath -> Handlers (LspM ())
handlers notesDir =
  mconcat
    [ notificationHandler SInitialized $ \_not -> do
        let params =
              ShowMessageRequestParams
                MtInfo
                "Turn on code lenses?"
                (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
        _ <- sendRequest SWindowShowMessageRequest params $ \case
          Right (Just (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)

            _ <- registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd' = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd') Nothing]
              responder (Right rsp)
            pure ()
          Right _ ->
            sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
          Left err ->
            sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> show err)
        pure (),
      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover ms (Just range)
            ms = HoverContents $ markedUpContent "markdown" ("**Notebook**:\n\n`" <> toText notesDir <> "`")
            range = Range pos pos
        responder (Right $ Just rsp)
    ]

lspServer :: App ()
lspServer = do
  notesDir <- getNotesDir
  void $
    liftIO $
      runServer $
        ServerDefinition
          { onConfigurationChange = const $ pure $ Right (),
            doInitialize = \env _req -> pure $ Right env,
            staticHandlers = handlers notesDir,
            interpretHandler = \env -> Iso (runLspT env) liftIO,
            options = defaultOptions
          }
