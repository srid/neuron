{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.ParserSpec
  ( spec,
  )
where

import Data.Default (def)
import Data.Some (Some (Some))
import Data.TagTree (Tag (Tag), mkTagPattern)
import qualified Network.URI.Encode as E
import Neuron.Zettelkasten.Connection (Connection (..))
import Neuron.Zettelkasten.ID (ZettelID (ZettelID))
import Neuron.Zettelkasten.Query.Parser (parseQueryLink)
import Neuron.Zettelkasten.Query.Theme (LinkView(LinkView_ShowDate), zettelsViewGroupByTag, zettelsViewLinkView, ZettelsView(ZettelsView))
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Relude
import Test.Hspec
import Text.URI (URI, mkURI)

spec :: Spec
spec = do
  -- The Markdown parser converts wiki-links to z: links, which should work.
  describe "z: links" $ do
    it "parses custom/hash ID" $ do
      parseQueryLink (asURI "z:/foo-bar")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (ZettelID "foo-bar") OrdinaryConnection)
    it "supports branching links" $ do
      parseQueryLink (asURI "z:/foo-bar?type=branch")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (ZettelID "foo-bar") Folgezettel)
    it "parses prefixed short link" $ do
      parseQueryLink (asURI "z:/foo-bar")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (ZettelID "foo-bar") OrdinaryConnection)
    it "resolves ambiguity using absolute URI" $ do
      parseQueryLink (asURI "z:/tags")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (ZettelID "tags") OrdinaryConnection)
    it "z:zettels" $ do
      parseQueryLink (asURI "z:zettels")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] Nothing OrdinaryConnection def)
    it "z:zettels?tag=foo" $ do
      parseQueryLink (asURI "z:zettels?tag=foo")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [mkTagPattern "foo"] Nothing OrdinaryConnection def)
    it "z:zettels?type=branch" $ do
      parseQueryLink (asURI "z:zettels?type=branch")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] Nothing Folgezettel def)
    it "z:zettels?limit=10" $ do
      parseQueryLink (asURI "z:zettels?limit=10")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] (Just 10) OrdinaryConnection def)
    it "z:zettels?timeline&limit=10" $ do
      parseQueryLink (asURI "z:zettels?timeline&limit=10")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] (Just 10) OrdinaryConnection ZettelsView{zettelsViewLinkView=LinkView_ShowDate, zettelsViewGroupByTag=False})
    it "z:zettels?limit=10&limit=20" $ do
      parseQueryLink (asURI "z:zettels?limit=10&limit=20")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] (Just 10) OrdinaryConnection def)
    it "z:tags" $ do
      parseQueryLink (asURI "z:tags")
        `shouldBe` (Just $ Some $ ZettelQuery_Tags [])
    it "z:tags?filter=foo" $ do
      parseQueryLink (asURI "z:tags?filter=foo")
        `shouldBe` (Just $ Some $ ZettelQuery_Tags [mkTagPattern "foo"])
    it "z:tag/foo" $ do
      parseQueryLink (asURI "z:tag/foo")
        `shouldBe` (Just $ Some $ ZettelQuery_TagZettel (Tag "foo"))
    it "z:tag/foo/bar/baz" $ do
      parseQueryLink (asURI "z:tag/foo/bar/baz")
        `shouldBe` (Just $ Some $ ZettelQuery_TagZettel (Tag "foo/bar/baz"))
  describe "z: links with i18n" $ do
    let encodeUriPath = toText . E.encode
    it "z:tag i18n" $ do
      parseQueryLink (asURI $ "z:tag/" <> encodeUriPath "日本語/おかもと先生/宿題")
        `shouldBe` (Just $ Some $ ZettelQuery_TagZettel (Tag "日本語/おかもと先生/宿題"))
    it "z:/ i18n" $ do
      parseQueryLink (asURI $ "z:/" <> encodeUriPath "计算机")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (ZettelID "计算机") OrdinaryConnection)

  describe "Regular Markdown links to zettels" $ do
    it "Supports full filename also" $ do
      parseQueryLink (asURI "foo-bar.md")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (ZettelID "foo-bar") OrdinaryConnection)

  describe "Regular Markdown links (non-zettels)" $ do
    it "But lets normal links pass through" $ do
      parseQueryLink (asURI "https://www.srid.ca")
        `shouldBe` Nothing
      parseQueryLink (asURI "/static/resume.pdf")
        `shouldBe` Nothing
      parseQueryLink (asURI "/static/")
        `shouldBe` Nothing
      parseQueryLink (asURI "/static")
        `shouldBe` Nothing

asURI :: Text -> URI
asURI s =
  either (error . toText . displayException) id $ mkURI s
