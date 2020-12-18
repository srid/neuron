{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.VersionSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Neuron.Version (neuronVersion, olderThan)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Application version" $ do
    it "should have dots" $ do
      neuronVersion `shouldSatisfy` T.isInfixOf "."
  -- TODO: Check minVersion in Default.dhall is same as the one in Paths_neuron
  describe "must compare" $ do
    let isGreater = shouldSatisfy
        isLesserOrEqual = shouldNotSatisfy
    it "simple versions" $ do
      -- If the user requires 0.4, and we are "older than" than that, fail (aka. isGreater)
      "1.3" `isGreater` olderThan
      "1.1" `isLesserOrEqual` olderThan -- This is current version
      "0.4" `isLesserOrEqual` olderThan
    it "full versions" $ do
      "1.2.1.2" `isGreater` olderThan
      "1.1.15" `isGreater` olderThan
      "1.1.14.8" `isGreater` olderThan
      "1.1.0.0" `isLesserOrEqual` olderThan -- This is current version
      "0.6.1.0" `isLesserOrEqual` olderThan
    it "within same major version" $ do
      "1.9.12.8" `isGreater` olderThan
      "1.1.0.0" `isLesserOrEqual` olderThan -- This is current version
