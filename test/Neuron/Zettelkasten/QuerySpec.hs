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
      parseQueryString "zquery://search?tag=foo" `shouldBe` Right [Query_ZettelsByTag $ TagPattern "foo"]
    it "Parse hierarchical tag" $ do
      parseQueryString "zquery://search?tag=foo/bar" `shouldBe` Right [Query_ZettelsByTag $ TagPattern "foo/bar"]
    it "Parse tag pattern" $ do
      parseQueryString "zquery://search?tag=foo/**/bar/*/baz" `shouldBe` Right [Query_ZettelsByTag $ TagPattern "foo/**/bar/*/baz"]
    it "Parse multiple tags" $
      parseQueryString "zquery://search?tag=foo&tag=bar"
        `shouldBe` Right [Query_ZettelsByTag $ TagPattern "foo", Query_ZettelsByTag $ TagPattern "bar"]
  where
    parseQueryString :: Text -> Either Text [Query]
    parseQueryString =
      either (Left . toText . displayException) queryFromURI . mkURI
