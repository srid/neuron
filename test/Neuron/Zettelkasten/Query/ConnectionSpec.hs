{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.ConnectionSpec
  ( spec,
  )
where

import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Connection
import Relude
import Test.Hspec
import Util

spec :: Spec
spec =
  describe "Connection parsing from URI" $ do
    it "handles z: connection type" $ do
      parseURIWith (liftE connectionFromURI) "z:/" `shouldBe` Right Folgezettel
    it "handles zcf: connection type" $ do
      parseURIWith (liftE connectionFromURI) "zcf:/" `shouldBe` Right OrdinaryConnection
    it "handles zquery: connection type" $ do
      parseURIWith (liftE connectionFromURI) "zquery:/" `shouldBe` Right Folgezettel
    it "handles zcfquery: connection type" $ do
      parseURIWith (liftE connectionFromURI) "zcfquery:/" `shouldBe` Right OrdinaryConnection
  where
    liftE :: (a -> b) -> a -> Either Text b
    liftE f =
      Right . f
