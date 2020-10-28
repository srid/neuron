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
import Neuron.Zettelkasten.Query.Parser (ZURILink (..), queryFromURILink)
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Relude
import Test.Hspec
import Text.Pandoc.Definition (Inline (Str))
import Text.URI (URI, mkURI)

spec :: Spec
spec = do
  -- The Markdown parser converts wiki-links to z: links, which should work.
  describe "z:/ links" $ do
    let zLink s = mkURILink s s
    it "parses custom/hash ID" $ do
      queryFromURILink (zLink "z:/foo-bar")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") Folgezettel)
    it "even with ?cf" $ do
      queryFromURILink (zLink "z:/foo-bar?cf")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
    it "parses prefixed short link" $ do
      queryFromURILink (zLink "z:/foo-bar")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") Folgezettel)
    it "resolves ambiguity using absolute URI" $ do
      queryFromURILink (zLink "z:/tags")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "tags") Folgezettel)
    it "z:zettels" $ do
      queryFromURILink (zLink "z:zettels")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] Folgezettel def)
    it "z:zettels?tag=foo" $ do
      queryFromURILink (zLink "z:zettels?tag=foo")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [mkTagPattern "foo"] Folgezettel def)
    it "z:zettels?cf" $ do
      queryFromURILink (zLink "z:zettels?cf")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] OrdinaryConnection def)
    it "z:tags" $ do
      queryFromURILink (zLink "z:tags")
        `shouldBe` (Just $ Some $ ZettelQuery_Tags [])
    it "z:tags?filter=foo" $ do
      queryFromURILink (zLink "z:tags?filter=foo")
        `shouldBe` (Just $ Some $ ZettelQuery_Tags [mkTagPattern "foo"])
    it "z:tag/foo" $ do
      queryFromURILink (zLink "z:tag/foo")
        `shouldBe` (Just $ Some $ ZettelQuery_TagZettel (Tag "foo"))
    it "z:tag/foo/bar/baz" $ do
      queryFromURILink (zLink "z:tag/foo/bar/baz")
        `shouldBe` (Just $ Some $ ZettelQuery_TagZettel (Tag "foo/bar/baz"))
    it "i18n" $ do
      let encodeUriPath = toText . E.encode . toString
          zLinkUnicode s = mkURILink' (mkURI . encodeUriPath) s s
      queryFromURILink (zLinkUnicode "z:/计算机")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "计算机") Folgezettel)
  let normalLink = mkURILink "some link text"
  describe "flexible links (regular markdown)" $ do
    it "Default connection type should be cf" $ do
      queryFromURILink (normalLink "z:/foo-bar")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
    it "Supports full filename instead of zettel ID" $ do
      queryFromURILink (normalLink "foo-bar.md")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
  describe "non-connection links" $ do
    it "pass through normal links" $ do
      queryFromURILink (normalLink "https://www.srid.ca")
        `shouldBe` Nothing
      queryFromURILink (normalLink "/static/resume.pdf")
        `shouldBe` Nothing
      queryFromURILink (normalLink "/static/")
        `shouldBe` Nothing
      queryFromURILink (normalLink "/static")
        `shouldBe` Nothing

mkURILink :: Text -> Text -> ZURILink
mkURILink =
  mkURILink' mkURI

mkURILink' ::
  (forall m. MonadThrow m => Text -> m URI) ->
  Text ->
  Text ->
  ZURILink
mkURILink' mk linkText s =
  -- TODO: Do this in reflex-dom-pandoc
  let uri = either (error . toText . displayException) id $ mk s
      inner = if linkText == s then Nothing else Just [Str linkText]
   in ZURILink inner uri
