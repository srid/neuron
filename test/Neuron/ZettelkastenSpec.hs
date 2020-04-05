{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.ZettelkastenSpec
  ( spec,
  )
where

import qualified Data.Text as T
import qualified Neuron.Zettelkasten as Z
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Application version" $ do
    -- TODO: More checks. This one is trivial and unnecessary, just for testing tests.
    it "should have dots" $ do
      Z.neuronVersion `shouldSatisfy` T.isInfixOf "."
    it "should contain the git rev" $ do
      pending
