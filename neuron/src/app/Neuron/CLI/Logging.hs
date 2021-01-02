{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Logging where

import Colog
  ( LogAction,
    Msg (Msg, msgSeverity, msgStack, msgText),
    cmap,
    logTextStderr,
  )
import Data.Ix (Ix)
import qualified Data.Text as T
import Relude
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (..),
    setSGRCode,
  )
import qualified Text.Show as Text

-- | Emoji prefix in log messages
data LogMoji
  = Done
  | Wait
  | WWW
  | Sent
  | Received
  deriving stock (Read, Eq, Ord, Enum, Bounded, Ix)

instance Text.Show LogMoji where
  show = \case
    Done ->
      "🥅"
    Wait ->
      "⏳"
    WWW ->
      "🌐"
    Sent ->
      "💾"
    Received ->
      "️📝"

-- | Severity with an optional emoji
data Severity
  = Debug (Maybe LogMoji)
  | Info (Maybe LogMoji)
  | Warning (Maybe LogMoji)
  | Error (Maybe LogMoji)
  deriving stock (Show, Read, Eq, Ord)

pattern D, I, W, E :: Severity
pattern D <- Debug Nothing where D = Debug Nothing
pattern I <- Info Nothing where I = Info Nothing
pattern W <- Warning Nothing where W = Warning Nothing
pattern E <- Error Nothing where E = Error Nothing

pattern D', I', W', E' :: LogMoji -> Severity
pattern D' le <- Debug (Just le) where D' le = Debug (Just le)
pattern I' le <- Info (Just le) where I' le = Info (Just le)
pattern W' le <- Warning (Just le) where W' le = Warning (Just le)
pattern E' le <- Error (Just le) where E' le = Error (Just le)

type Message = Msg Severity

mkLogAction :: (MonadIO m) => LogAction m Message
mkLogAction =
  cmap fmtNeuronMsg logTextStderr
  where
    fmtNeuronMsg :: Message -> Text
    fmtNeuronMsg Msg {..} =
      let emptyEmoji = "  " -- Two spaces, because our emojis spans two ascii letters
          f c mle = color c $ maybe emptyEmoji show mle <> " " <> msgText
       in case msgSeverity of
            Debug mle -> f Black mle
            Info mle -> f Blue mle
            Warning mle -> f Yellow mle
            Error mle -> f Red mle
    color :: Color -> Text -> Text
    color c txt =
      T.pack (setSGRCode [SetColor Foreground Vivid c])
        <> txt
        <> T.pack (setSGRCode [Reset])
