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
    -- TODO: More checks. This one is trivial and unnecessary, just for testing tests.
    it "should have dots" $ do
      neuronVersion `shouldSatisfy` T.isInfixOf "."
    it "should contain the git rev" $ do
      pending
  describe "Version comparison" $ do
    it "must compare simple versions" $ do
      "0.2" `shouldSatisfy` olderThan
      "0.1" `shouldNotSatisfy` olderThan -- This is current version
      "0.0" `shouldNotSatisfy` olderThan
    it "must compare full versions" $ do
      "0.2.1.2" `shouldSatisfy` olderThan
      "0.2.3" `shouldSatisfy` olderThan
      "0.1.0.0" `shouldNotSatisfy` olderThan -- This is current version
      "0.0.1.0" `shouldNotSatisfy` olderThan
    it "must compare within same major version" $ do
      "0.1.0.2" `shouldSatisfy` olderThan -- 0.1.0.0 is the current version
