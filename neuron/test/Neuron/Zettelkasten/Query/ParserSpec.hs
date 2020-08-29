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
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Pandoc.URILink
import Relude
import Test.Hspec
import Text.Pandoc.Definition
import Text.URI

spec :: Spec
spec = do
  describe "short links" $ do
    let shortLink s = mkURILink s s
    it "parses date ID" $ do
      queryFromURILink (shortLink "1234567")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "1234567") Folgezettel)
    it "parses custom/hash ID" $ do
      queryFromURILink (shortLink "foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "foo-bar") Folgezettel)
    it "even with ?cf" $ do
      queryFromURILink (shortLink "foo-bar?cf")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "foo-bar") OrdinaryConnection)
    it "parses prefixed short link" $ do
      queryFromURILink (shortLink "z:/foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "foo-bar") Folgezettel)
    it "resolves ambiguity using absolute URI" $ do
      queryFromURILink (shortLink "z:/tags")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "tags") Folgezettel)
    it "z:zettels" $ do
      queryFromURILink (shortLink "z:zettels")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelsByTag [] Folgezettel def)
    it "z:zettels?tag=foo" $ do
      queryFromURILink (shortLink "z:zettels?tag=foo")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelsByTag [mkTagPattern "foo"] Folgezettel def)
    it "z:zettels?cf" $ do
      queryFromURILink (shortLink "z:zettels?cf")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelsByTag [] OrdinaryConnection def)
    it "z:tags" $ do
      queryFromURILink (shortLink "z:tags")
        `shouldBe` Right (Just $ Some $ ZettelQuery_Tags [])
    it "z:tags?filter=foo" $ do
      queryFromURILink (shortLink "z:tags?filter=foo")
        `shouldBe` Right (Just $ Some $ ZettelQuery_Tags [mkTagPattern "foo"])
  describe "flexible links (regular markdown)" $ do
    let normalLink = mkURILink "some link text"
    it "Default connection type should be cf" $ do
      queryFromURILink (normalLink "foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (parseZettelID "foo-bar") OrdinaryConnection)

mkURILink :: Text -> Text -> URILink
mkURILink linkText s =
  -- TODO: Do this in reflex-dom-pandoc
  let uri = either (error . toText . displayException) id $ mkURI s
      isAutoLink = linkText == s
   in URILink [Str linkText] uri isAutoLink
