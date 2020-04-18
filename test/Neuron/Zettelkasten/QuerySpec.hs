{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.QuerySpec
  ( spec,
  )
where

import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Tag
import Relude
import Test.Hspec
import Text.URI (mkURI)

spec :: Spec
spec =
  describe "Parse query URI" $ do
    it "Parse all zettels URI" $
      parseQueryString "zquery://search" `shouldBe` Right []
    it "Parse single tag" $
      parseQueryString "zquery://search?tag=foo" `shouldBe` Right [ByTag $ TagPattern "foo"]
    it "Parse hierarchical tag" $ do
      parseQueryString "zquery://search?tag=foo/bar" `shouldBe` Right [ByTag $ TagPattern "foo/bar"]
    it "Parse tag pattern" $ do
      parseQueryString "zquery://search?tag=foo/**/bar/*/baz" `shouldBe` Right [ByTag $ TagPattern "foo/**/bar/*/baz"]
    it "Parse multiple tags" $
      parseQueryString "zquery://search?tag=foo&tag=bar"
        `shouldBe` Right [ByTag $ TagPattern "foo", ByTag $ TagPattern "bar"]
  where
    parseQueryString =
      bimap displayException queryFromURI . mkURI
