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
    it "must compare correctly" $ do
      "0.2" `shouldSatisfy` olderThan
      "0.1" `shouldNotSatisfy` olderThan -- This is current version
      "0.0" `shouldNotSatisfy` olderThan
