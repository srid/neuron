{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.QuerySpec
  ( spec,
  )
where

import Data.Some
import Neuron.Zettelkasten.ID
import Text.MMark.MarkdownLink
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Tag
import Relude
import Test.Hspec
import Text.URI
import Util

spec :: Spec
spec = do
  describe "Parse zettels by tag URIs" $ do
    let zettelsByTag = Some . Query_ZettelsByTag . fmap mkTagPattern
    it "Parse all zettels URI" $
      parseURIWith queryFromURI "zquery://search" `shouldBe` Right (zettelsByTag [])
    it "Parse single tag" $
      parseURIWith queryFromURI "zquery://search?tag=foo" `shouldBe` Right (zettelsByTag ["foo"])
    it "Parse hierarchical tag" $ do
      parseURIWith queryFromURI "zquery://search?tag=foo/bar" `shouldBe` Right (zettelsByTag ["foo/bar"])
    it "Parse tag pattern" $ do
      parseURIWith queryFromURI "zquery://search?tag=foo/**/bar/*/baz" `shouldBe` Right (zettelsByTag ["foo/**/bar/*/baz"])
    it "Parse multiple tags" $
      parseURIWith queryFromURI "zquery://search?tag=foo&tag=bar"
        `shouldBe` Right (zettelsByTag ["foo", "bar"])
  describe "Parse zettels by ID URI" $ do
    let zid = parseZettelID "1234567"
        zettelById = Some . Query_ZettelByID
    it "parses z:/" $
      queryFromMarkdownLink (mkMarkdownLink "1234567" "z:/")
        `shouldBe` Right (Just $ zettelById zid)
    it "parses z:/ ignoring annotation" $
      queryFromMarkdownLink (mkMarkdownLink "1234567" "z://foo-bar")
        `shouldBe` Right (Just $ zettelById zid)
    it "parses zcf:/" $
      queryFromMarkdownLink (mkMarkdownLink "1234567" "zcf:/")
        `shouldBe` Right (Just $ zettelById zid)
  describe "Parse tags URI" $ do
    it "parses zquery://tags" $
      queryFromMarkdownLink (mkMarkdownLink "." "zquery://tags?filter=foo/**")
        `shouldBe` Right (Just $ Some $ Query_Tags [mkTagPattern "foo/**"])

mkMarkdownLink :: Text -> Text -> MarkdownLink
mkMarkdownLink s l =
  MarkdownLink s $ either (error . toText . displayException) id $ mkURI l
