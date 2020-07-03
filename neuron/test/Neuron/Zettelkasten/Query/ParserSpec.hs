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
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Parser
import Neuron.Zettelkasten.Query.Theme
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Pandoc.URILink
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
              Right $ Just $ Some $ ZettelQuery_ZettelsByTag (fmap mkTagPattern pat) mconn mview
            withScheme s = toText scheme <> s
            legacyLink l = mkURILink "." l
        it "Parse all zettels URI" $ do
          queryFromURILink (legacyLink $ withScheme "://search")
            `shouldBe` zettelsByTag [] def
        it "Parse single tag" $
          queryFromURILink (legacyLink $ withScheme "://search?tag=foo")
            `shouldBe` zettelsByTag ["foo"] def
        it "Parse hierarchical tag" $ do
          queryFromURILink (legacyLink $ withScheme "://search?tag=foo/bar")
            `shouldBe` zettelsByTag ["foo/bar"] def
        it "Parse tag pattern" $ do
          queryFromURILink (legacyLink $ withScheme "://search?tag=foo/**/bar/*/baz")
            `shouldBe` zettelsByTag ["foo/**/bar/*/baz"] def
        it "Parse multiple tags" $
          queryFromURILink (legacyLink $ withScheme "://search?tag=foo&tag=bar")
            `shouldBe` zettelsByTag ["foo", "bar"] def
        it "Handles ?grouped" $ do
          queryFromURILink (legacyLink $ withScheme "://search?grouped")
            `shouldBe` zettelsByTag [] (ZettelsView def True)
        it "Handles ?linkTheme=withDate" $ do
          queryFromURILink (legacyLink $ withScheme "://search?linkTheme=withDate")
            `shouldBe` zettelsByTag [] (ZettelsView (LinkView True) False)
  describe "Parse zettels by ID URI" $ do
    let zid = parseZettelID "1234567"
    it "parses z:/" $
      queryFromURILink (mkURILink "1234567" "z:/")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID zid Nothing)
    it "parses z:/ ignoring annotation" $
      queryFromURILink (mkURILink "1234567" "z://foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID zid Nothing)
    it "parses zcf:/" $
      queryFromURILink (mkURILink "1234567" "zcf:/")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID zid (Just OrdinaryConnection))
  describe "Parse tags URI" $ do
    it "parses zquery://tags" $
      queryFromURILink (mkURILink "." "zquery://tags?filter=foo/**")
        `shouldBe` Right (Just $ Some $ ZettelQuery_Tags [mkTagPattern "foo/**"])

shortLinks :: Spec
shortLinks = do
  describe "short links" $ do
    let shortLink s = mkURILink s s
    it "parses date ID" $ do
      queryFromURILink (shortLink "1234567")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "1234567") Nothing)
    it "parses custom/hash ID" $ do
      queryFromURILink (shortLink "foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "foo-bar") Nothing)
    it "even with ?cf" $ do
      queryFromURILink (shortLink "foo-bar?cf")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "foo-bar") (Just OrdinaryConnection))
    it "parses prefixed short link" $ do
      queryFromURILink (shortLink "z:/foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "foo-bar") Nothing)
    it "z:zettels" $ do
      queryFromURILink (shortLink "z:zettels")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelsByTag [] Nothing def)
    it "z:zettels?tag=foo" $ do
      queryFromURILink (shortLink "z:zettels?tag=foo")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelsByTag [mkTagPattern "foo"] Nothing def)
    it "z:zettels?cf" $ do
      queryFromURILink (shortLink "z:zettels?cf")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelsByTag [] (Just OrdinaryConnection) def)
    it "z:tags" $ do
      queryFromURILink (shortLink "z:tags")
        `shouldBe` Right (Just $ Some $ ZettelQuery_Tags [])
    it "z:tags?filter=foo" $ do
      queryFromURILink (shortLink "z:tags?filter=foo")
        `shouldBe` Right (Just $ Some $ ZettelQuery_Tags [mkTagPattern "foo"])

mkURILink :: Text -> Text -> URILink
mkURILink s l =
  URILink s $ either (error . toText . displayException) id $ mkURI l
