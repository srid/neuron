{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.ParserSpec
  ( spec,
  )
where

import Data.Default (def)
import Data.Some
import Data.TagTree
import Neuron.Markdown
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Parser
import Neuron.Zettelkasten.Query.Theme
import Relude
import Test.Hspec
import Text.URI

spec :: Spec
spec = do
  legacyLinks
  shortLinks

legacyLinks :: Spec
legacyLinks = do
  describe "Parse zettels by tag URIs" $ do
    for_ [("zquery", Nothing), ("zcfquery", Just OrdinaryConnection)] $ \(scheme, mconn) -> do
      context scheme $ do
        let zettelsByTag pat mview =
              Right $ Just $ Some $ Query_ZettelsByTag (fmap mkTagPattern pat) mconn mview
            withScheme s = toText scheme <> s
            legacyLink l = mkMarkdownLink "." l
        it "Parse all zettels URI" $ do
          queryFromMarkdownLink (legacyLink $ withScheme "://search")
            `shouldBe` zettelsByTag [] def
        it "Parse single tag" $
          queryFromMarkdownLink (legacyLink $ withScheme "://search?tag=foo")
            `shouldBe` zettelsByTag ["foo"] def
        it "Parse hierarchical tag" $ do
          queryFromMarkdownLink (legacyLink $ withScheme "://search?tag=foo/bar")
            `shouldBe` zettelsByTag ["foo/bar"] def
        it "Parse tag pattern" $ do
          queryFromMarkdownLink (legacyLink $ withScheme "://search?tag=foo/**/bar/*/baz")
            `shouldBe` zettelsByTag ["foo/**/bar/*/baz"] def
        it "Parse multiple tags" $
          queryFromMarkdownLink (legacyLink $ withScheme "://search?tag=foo&tag=bar")
            `shouldBe` zettelsByTag ["foo", "bar"] def
        it "Handles ?grouped" $ do
          queryFromMarkdownLink (legacyLink $ withScheme "://search?grouped")
            `shouldBe` zettelsByTag [] (ZettelsView def True)
        it "Handles ?linkTheme=withDate" $ do
          queryFromMarkdownLink (legacyLink $ withScheme "://search?linkTheme=withDate")
            `shouldBe` zettelsByTag [] (ZettelsView (LinkView True) False)
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

shortLinks :: Spec
shortLinks = do
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
    it "z:zettels" $ do
      queryFromMarkdownLink (shortLink "z:zettels")
        `shouldBe` Right (Just $ Some $ Query_ZettelsByTag [] Nothing def)
    it "z:zettels?tag=foo" $ do
      queryFromMarkdownLink (shortLink "z:zettels?tag=foo")
        `shouldBe` Right (Just $ Some $ Query_ZettelsByTag [mkTagPattern "foo"] Nothing def)
    it "z:zettels?cf" $ do
      queryFromMarkdownLink (shortLink "z:zettels?cf")
        `shouldBe` Right (Just $ Some $ Query_ZettelsByTag [] (Just OrdinaryConnection) def)
    it "z:tags" $ do
      queryFromMarkdownLink (shortLink "z:tags")
        `shouldBe` Right (Just $ Some $ Query_Tags [])
    it "z:tags?filter=foo" $ do
      queryFromMarkdownLink (shortLink "z:tags?filter=foo")
        `shouldBe` Right (Just $ Some $ Query_Tags [mkTagPattern "foo"])

mkMarkdownLink :: Text -> Text -> MarkdownLink
mkMarkdownLink s l =
  MarkdownLink s $ either (error . toText . displayException) id $ mkURI l
