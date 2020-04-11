{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.IDSpec
  ( spec,
  )
where

import Data.Time.Calendar
import qualified Neuron.Zettelkasten.ID as Z
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Zettel ID" $ do
    context "date id parsing" $ do
      let day = fromGregorian 2020 3 19
          zid = Z.ZettelDateID day 1
      it "parses a zettel ID" $ do
        Z.parseZettelID "2011401" `shouldBe` zid
      it "parses a zettel ID from zettel filename" $ do
        Z.mkZettelID "2011401.md" `shouldBe` zid
        Z.zettelIDSourceFileName zid `shouldBe` "2011401.md"
      it "returns the correct day" $ do
        Z.zettelIDDay zid `shouldBe` Just day
    context "custom id parsing" $ do
      let zid = Z.ZettelCustomID "20abcde"
      it "parses a custom zettel ID" $ do
        Z.parseZettelID' "20abcde" `shouldBe` Right zid
      it "parses a custom zettel ID from zettel filename" $ do
        Z.mkZettelID "20abcde.md" `shouldBe` zid
        Z.zettelIDSourceFileName zid `shouldBe` "20abcde.md"
