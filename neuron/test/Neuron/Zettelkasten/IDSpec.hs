{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.IDSpec
  ( spec,
  )
where

import qualified Data.Aeson as Aeson
import Data.Time.Calendar
import qualified Neuron.Reader.Type as Z
import qualified Neuron.Zettelkasten.ID as Z
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "ID parsing" $ do
    let day = fromGregorian 2020 3 19
    context "date id parsing" $ do
      let zid = Z.ZettelDateID day 1
      it "parses a zettel ID" $ do
        Z.parseZettelID "2011401" `shouldBe` Right zid
      it "parses a zettel ID from zettel filename" $ do
        Z.getZettelID Z.ZettelFormat_Markdown "2011401.md" `shouldBe` Just zid
        Z.zettelIDSourceFileName zid Z.ZettelFormat_Markdown `shouldBe` "2011401.md"
    context "custom id parsing" $ do
      let zid = Z.ZettelCustomID "20abcde"
      it "parses a custom zettel ID" $ do
        Z.parseZettelID "20abcde" `shouldBe` Right zid
      it "parses a custom zettel ID from zettel filename" $ do
        Z.getZettelID Z.ZettelFormat_Markdown "20abcde.md" `shouldBe` Just zid
        Z.zettelIDSourceFileName zid Z.ZettelFormat_Markdown `shouldBe` "20abcde.md"
      let deceptiveZid = Z.ZettelCustomID "2136537e"
      it "parses a custom zettel ID that looks like date ID" $ do
        Z.parseZettelID "2136537e" `shouldBe` Right deceptiveZid
      it "parses a custom zettel ID with dot" $ do
        Z.parseZettelID "foo.bar" `shouldBe` Right (Z.ZettelCustomID "foo.bar")
        -- Even if there is a ".md" (not a file extension)
        Z.parseZettelID "foo.md" `shouldBe` Right (Z.ZettelCustomID "foo.md")
    context "failures" $ do
      it "fails to parse ID with disallowed characters" $ do
        Z.parseZettelID "/foo" `shouldSatisfy` isLeft
        Z.parseZettelID "foo$" `shouldSatisfy` isLeft
        Z.parseZettelID "foo bar" `shouldSatisfy` isLeft
  describe "ID converstion" $ do
    context "JSON encoding" $ do
      let day = fromGregorian 2020 3 19
          zid = Z.ZettelDateID day 1
      it "Converts ID to text when encoding to JSON" $ do
        Aeson.toJSON (Z.ZettelCustomID "20abcde") `shouldBe` Aeson.String "20abcde"
        Aeson.toJSON zid `shouldBe` Aeson.String "2011401"
    it "Date ID to Text" $ do
      let day = fromGregorian 2020 4 21
      Z.zettelIDText (Z.ZettelDateID day 1) `shouldBe` "2016201"
