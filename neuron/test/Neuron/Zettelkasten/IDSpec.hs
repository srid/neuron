{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.IDSpec
  ( spec,
  )
where

import qualified Data.Aeson as Aeson
import qualified Neuron.Reader.Type as Z
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
        Z.getZettelID Z.ZettelFormat_Markdown "20abcde.md" `shouldBe` Just zid
        Z.zettelIDSourceFileName zid Z.ZettelFormat_Markdown `shouldBe` "20abcde.md"
      let deceptiveZid = Z.ZettelID "2136537e"
      it "parses a custom zettel ID that looks like date ID" $ do
        Z.parseZettelID "2136537e" `shouldBe` Right deceptiveZid
      it "parses a custom zettel ID with dot" $ do
        Z.parseZettelID "foo.bar" `shouldBe` Right (Z.ZettelID "foo.bar")
        -- Even if there is a ".md" (not a file extension)
        Z.parseZettelID "foo.md" `shouldBe` Right (Z.ZettelID "foo.md")
    context "failures" $ do
      it "fails to parse ID with disallowed characters" $ do
        Z.parseZettelID "/foo" `shouldSatisfy` isLeft
        Z.parseZettelID "foo$" `shouldSatisfy` isLeft
        Z.parseZettelID "foo bar" `shouldSatisfy` isLeft
  describe "ID converstion" $ do
    context "JSON encoding" $ do
      it "Converts ID to text when encoding to JSON" $ do
        Aeson.toJSON (Z.ZettelID "20abcde") `shouldBe` Aeson.String "20abcde"
