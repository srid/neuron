{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.ConnectionSpec
  ( spec,
  )
where

import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Query.Connection
import Relude
import Test.Hspec
import Text.MMark.MarkdownLink
import Text.URI

spec :: Spec
spec = do
  describe "Connection parsing from URI" $ do
    let mkLink = mkMarkdownLink "."
    it "handles z: connection type" $ do
      connectionFromMarkdownLink (mkLink "z:/") `shouldBe` Folgezettel
    it "handles zcf: connection type" $ do
      connectionFromMarkdownLink (mkLink "zcf:/") `shouldBe` OrdinaryConnection
    it "handles zquery: connection type" $ do
      connectionFromMarkdownLink (mkLink "zquery:/") `shouldBe` Folgezettel
    it "handles zcfquery: connection type" $ do
      connectionFromMarkdownLink (mkLink "zcfquery:/") `shouldBe` OrdinaryConnection
  describe "Connection parsing from short links" $ do
    let shortLink s = mkMarkdownLink s s
    it "handles basic short links" $ do
      connectionFromMarkdownLink (shortLink "1234567")
        `shouldBe` Folgezettel
    it "handles cf short links" $ do
      connectionFromMarkdownLink (shortLink "1234567?cf")
        `shouldBe` OrdinaryConnection

mkMarkdownLink :: Text -> Text -> MarkdownLink
mkMarkdownLink s l =
  MarkdownLink s $ either (error . toText . displayException) id $ mkURI l
