{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.QuerySpec
  ( spec,
  )
where

import Data.Some
import Data.TagTree
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query
import Relude
import Test.Hspec
import Text.MMark.MarkdownLink
import Text.URI
import Util

spec :: Spec
spec = do
  describe "Parse zettels by tag URIs" $ do
    it "link theme" $ pendingWith "TODO: parametrize below"
    for_ [("zquery", Nothing), ("zcfquery", Just OrdinaryConnection)] $ \(scheme, mconn) -> do
      context scheme $ do
        let zettelsByTag pat =
              Right $ Just $ Some $ Query_ZettelsByTag (fmap mkTagPattern pat) mconn Nothing
            withScheme s = toText scheme <> s
        it "Parse all zettels URI" $ do
          parseURIWith queryFromURI (withScheme "://search")
            `shouldBe` zettelsByTag []
        it "Parse single tag" $
          parseURIWith queryFromURI (withScheme "://search?tag=foo")
            `shouldBe` zettelsByTag ["foo"]
        it "Parse hierarchical tag" $ do
          parseURIWith queryFromURI (withScheme "://search?tag=foo/bar")
            `shouldBe` zettelsByTag ["foo/bar"]
        it "Parse tag pattern" $ do
          parseURIWith queryFromURI (withScheme "://search?tag=foo/**/bar/*/baz")
            `shouldBe` zettelsByTag ["foo/**/bar/*/baz"]
        it "Parse multiple tags" $
          parseURIWith queryFromURI (withScheme "://search?tag=foo&tag=bar")
            `shouldBe` zettelsByTag ["foo", "bar"]
  describe "Parse zettels by ID URI" $ do
    let zid = parseZettelID "1234567"
    it "parses z:/" $
      queryFromMarkdownLink (mkMarkdownLink "1234567" "z:/")
        `shouldBe` Right (Just $ Some $ Query_ZettelByID zid Nothing)
    it "parses z:/ ignoring annotation" $
      queryFromMarkdownLink (mkMarkdownLink "1234567" "z://foo-bar")
        `shouldBe` Right (Just $ Some $ Query_ZettelByID zid Nothing)
    it "parses zcf:/" $
      queryFromMarkdownLink (mkMarkdownLink "1234567" "zcf:/")
        `shouldBe` Right (Just $ Some $ Query_ZettelByID zid (Just OrdinaryConnection))
  describe "Parse tags URI" $ do
    it "parses zquery://tags" $
      queryFromMarkdownLink (mkMarkdownLink "." "zquery://tags?filter=foo/**")
        `shouldBe` Right (Just $ Some $ Query_Tags [mkTagPattern "foo/**"])
  describe "short links" $ do
    let shortLink s = mkMarkdownLink s s
    it "parses date ID" $ do
      queryFromMarkdownLink (shortLink "1234567")
        `shouldBe` Right (Just $ Some $ Query_ZettelByID (parseZettelID "1234567") Nothing)
    it "parses custom/hash ID" $ do
      queryFromMarkdownLink (shortLink "foo-bar")
        `shouldBe` Right (Just $ Some $ Query_ZettelByID (parseZettelID "foo-bar") Nothing)
    it "even with ?cf" $ do
      queryFromMarkdownLink (shortLink "foo-bar?cf")
        `shouldBe` Right (Just $ Some $ Query_ZettelByID (parseZettelID "foo-bar") (Just OrdinaryConnection))

mkMarkdownLink :: Text -> Text -> MarkdownLink
mkMarkdownLink s l =
  MarkdownLink s $ either (error . toText . displayException) id $ mkURI l
