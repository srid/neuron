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
    it "must compare simple versions" $ do
      "0.3" `shouldSatisfy` olderThan
      "0.2" `shouldNotSatisfy` olderThan -- This is current version
      "0.1" `shouldNotSatisfy` olderThan
    it "must compare full versions" $ do
      "0.3.1.2" `shouldSatisfy` olderThan
      "0.3.3" `shouldSatisfy` olderThan
      "0.2.0.0" `shouldNotSatisfy` olderThan -- This is current version
      "0.1.1.0" `shouldNotSatisfy` olderThan
    it "must compare within same major version" $ do
      "0.2.0.2" `shouldSatisfy` olderThan -- This is current version
