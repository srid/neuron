{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.VersionSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Neuron.Version
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Application version" $ do
    it "should have dots" $ do
      neuronVersion `shouldSatisfy` T.isInfixOf "."
    it "should contain the git rev" $ do
      pending
  -- TODO: Check minVersion in Default.dhall is same as the one in Paths_neuron
  describe "Version comparison" $ do
    let isGreater = shouldSatisfy
        isLesserOrEqual = shouldNotSatisfy
    it "must compare simple versions" $ do
      -- If the user requires 0.4, and we are "older than" than that, fail (aka. isGreater)
      "0.4" `isGreater` olderThan
      "0.3" `isLesserOrEqual` olderThan -- This is current version
      "0.2" `isLesserOrEqual` olderThan
    it "must compare full versions" $ do
      "0.4.1.2" `isGreater` olderThan
      "0.4.3" `isGreater` olderThan
      "0.3.1.8" `isGreater` olderThan
      "0.3.1.0" `isLesserOrEqual` olderThan -- This is current version
      "0.2.1.0" `isLesserOrEqual` olderThan
    it "must compare within same major version" $ do
      "0.3.1.8" `isGreater` olderThan
      "0.3.1.0" `isLesserOrEqual` olderThan -- This is current version
