{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.ParserSpec
  ( spec,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Default (def)
import Data.Some (Some (Some))
import Data.TagTree (Tag (Tag), mkTagPattern)
import qualified Network.URI.Encode as E
import Neuron.Zettelkasten.Connection (Connection (..))
import Neuron.Zettelkasten.ID (unsafeMkZettelID)
import Neuron.Zettelkasten.Query.Parser (queryFromURILink)
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Reflex.Dom.Pandoc.URILink (URILink (URILink))
import Relude
import Test.Hspec
import Text.Pandoc.Definition (Inline (Str))
import Text.URI (URI, mkURI)

spec :: Spec
spec = do
  describe "short links" $ do
    let shortLink s = mkURILink s s
    it "parses custom/hash ID" $ do
      queryFromURILink (shortLink "foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") Folgezettel)
    it "even with ?cf" $ do
      queryFromURILink (shortLink "foo-bar?cf")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
    it "parses prefixed short link" $ do
      queryFromURILink (shortLink "z:/foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") Folgezettel)
    it "resolves ambiguity using absolute URI" $ do
      queryFromURILink (shortLink "z:/tags")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "tags") Folgezettel)
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
    it "z:tag/foo" $ do
      queryFromURILink (shortLink "z:tag/foo")
        `shouldBe` Right (Just $ Some $ ZettelQuery_TagZettel (Tag "foo"))
    it "z:tag/foo/bar/baz" $ do
      queryFromURILink (shortLink "z:tag/foo/bar/baz")
        `shouldBe` Right (Just $ Some $ ZettelQuery_TagZettel (Tag "foo/bar/baz"))
    it "i18n" $ do
      let encodeUriPath = toText . E.encode . toString
          shortLinkUnicode s = mkURILink' (mkURI . encodeUriPath) s s
      queryFromURILink (shortLinkUnicode "计算机")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "计算机") Folgezettel)
  let normalLink = mkURILink "some link text"
  describe "flexible links (regular markdown)" $ do
    it "Default connection type should be cf" $ do
      queryFromURILink (normalLink "foo-bar")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
    it "Supports full filename instead of zettel ID" $ do
      queryFromURILink (normalLink "foo-bar.md")
        `shouldBe` Right (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
  describe "non-connection links" $ do
    it "pass through normal links" $ do
      queryFromURILink (normalLink "https://www.srid.ca")
        `shouldBe` Right Nothing
      queryFromURILink (normalLink "/static/resume.pdf")
        `shouldBe` Right Nothing
      queryFromURILink (normalLink "/static/")
        `shouldBe` Right Nothing
      queryFromURILink (normalLink "/static")
        `shouldBe` Right Nothing

mkURILink :: Text -> Text -> URILink
mkURILink =
  mkURILink' mkURI

mkURILink' ::
  (forall m. MonadThrow m => Text -> m URI) ->
  Text ->
  Text ->
  URILink
mkURILink' mk linkText s =
  -- TODO: Do this in reflex-dom-pandoc
  let uri = either (error . toText . displayException) id $ mk s
      inner = if linkText == s then Nothing else Just [Str linkText]
   in URILink inner uri
