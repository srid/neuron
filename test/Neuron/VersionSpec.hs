{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.VersionSpec
  ( spec,
  )
where

import Data.Tagged
import qualified Data.Text as T
import Neuron.Version (neuronVersion)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Application version" $ do
    it "should have dots" $ do
      untag neuronVersion `shouldSatisfy` T.isInfixOf "."
