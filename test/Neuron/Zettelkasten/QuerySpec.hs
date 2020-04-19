{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.QuerySpec
  ( spec,
  )
where

import Data.Some
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Tag
import Relude
import Test.Hspec
import Text.URI (mkURI)

spec :: Spec
spec =
  describe "Parse query URI" $ do
    let zettelsByTag = Some . Query_ZettelsByTag . fmap mkTagPattern
    it "Parse all zettels URI" $
      parseQueryString "zquery://search" `shouldBe` Right (zettelsByTag [])
    it "Parse single tag" $
      parseQueryString "zquery://search?tag=foo" `shouldBe` Right (zettelsByTag ["foo"])
    it "Parse hierarchical tag" $ do
      parseQueryString "zquery://search?tag=foo/bar" `shouldBe` Right (zettelsByTag ["foo/bar"])
    it "Parse tag pattern" $ do
      parseQueryString "zquery://search?tag=foo/**/bar/*/baz" `shouldBe` Right (zettelsByTag ["foo/**/bar/*/baz"])
    it "Parse multiple tags" $
      parseQueryString "zquery://search?tag=foo&tag=bar"
        `shouldBe` Right (zettelsByTag ["foo", "bar"])
  where
    parseQueryString :: Text -> Either Text (Some Query)
    parseQueryString =
      either (Left . toText . displayException) queryFromURI . mkURI
