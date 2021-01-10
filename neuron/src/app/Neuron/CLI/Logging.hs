{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Logging where

import Colog
  ( LogAction,
    Msg (Msg, msgSeverity, msgStack, msgText),
    cmap,
    logTextStderr,
  )
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
  | -- | Custom string can be used to substitute for an emoji.
    -- It must be of length two, inasmuch as many emojis take two-letter space.
    Custom Char Char
  deriving stock (Read, Eq, Ord)

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
    Custom a b ->
      [a, b]

-- | Severity with an optional emoji
data Severity
  = Debug (Maybe LogMoji)
  | Info (Maybe LogMoji)
  | Warning (Maybe LogMoji)
  | Error (Maybe LogMoji)
  deriving stock (Show, Read, Eq, Ord)

pattern D, I, W, E, EE :: Severity
pattern D <- Debug Nothing where D = Debug Nothing
pattern I <- Info Nothing where I = Info Nothing
pattern W <- Warning Nothing where W = Warning Nothing
pattern E <- Error Nothing where E = Error Nothing
pattern EE <- Error (Just (Custom '!' '!')) where EE = Error (Just $ Custom '!' '!')

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
      let emptyEmoji = Custom ' ' ' '
          f c (fromMaybe emptyEmoji -> le) = color c $ show le <> " " <> msgText
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

indentAllButFirstLine :: Int -> Text -> Text
indentAllButFirstLine n = T.strip . unlines . go . lines
  where
    go [] = []
    go [x] = [x]
    go (x : xs) =
      x : fmap (toText . (replicate n ' ' <>) . toString) xs
