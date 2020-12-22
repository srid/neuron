{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.IDSpec
  ( spec,
  )
where

import qualified Data.Aeson as Aeson
import qualified Neuron.Zettelkasten.ID as Z
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "ID parsing" $ do
    context "custom id parsing" $ do
      let zid = Z.ZettelID "20abcde"
      it "parses a custom zettel ID" $ do
        Z.parseZettelID "20abcde" `shouldBe` Right zid
      it "parses a custom zettel ID from zettel filename" $ do
        Z.getZettelID "20abcde.md" `shouldBe` Just zid
        Z.zettelIDSourceFileName zid `shouldBe` "20abcde.md"
      let deceptiveZid = Z.ZettelID "2136537e"
      it "parses a custom zettel ID that looks like date ID" $ do
        Z.parseZettelID "2136537e" `shouldBe` Right deceptiveZid
      it "parses a custom zettel ID with dot" $ do
        Z.parseZettelID "foo.bar" `shouldBe` Right (Z.ZettelID "foo.bar")
        -- Even if there is a ".md" (not a file extension)
        Z.parseZettelID "foo.md" `shouldBe` Right (Z.ZettelID "foo.md")
      it "parses full-phrase IDs" $ do
        Z.parseZettelID "foo bar" `shouldBe` Right (Z.ZettelID "foo bar")
    context "i18n" $ do
      it "deals with unicode chars" $ do
        Z.parseZettelID "计算机" `shouldBe` Right (Z.ZettelID "计算机")
    context "failures" $ do
      it "fails to parse ID with disallowed characters" $ do
        Z.parseZettelID "/foo" `shouldSatisfy` isLeft
        Z.parseZettelID "foo$" `shouldSatisfy` isLeft
  describe "ID converstion" $ do
    context "JSON encoding" $ do
      it "Converts ID to text when encoding to JSON" $ do
        Aeson.toJSON (Z.ZettelID "20abcde") `shouldBe` Aeson.String "20abcde"
