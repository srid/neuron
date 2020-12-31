{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import GHC.IO.Handle (BufferMode (LineBuffering))
import Main.Utf8 (withUtf8)
import Neuron.CLI (run)
import qualified Neuron.Reactor as Reactor
import Relude
import System.IO (hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  withUtf8 $
    run Reactor.generateSite
