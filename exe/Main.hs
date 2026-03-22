{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Main.Utf8 (withUtf8)
import Neuron.CLI.App (run)
import qualified Neuron.Reactor as Reactor
import Relude

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  withUtf8 $ do
    run Reactor.generateSite
