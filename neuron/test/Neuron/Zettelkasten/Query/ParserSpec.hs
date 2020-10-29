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
import Neuron.Zettelkasten.ID (unsafeMkZettelID)
import Neuron.Zettelkasten.Query.Parser (queryFromPandocLink)
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Relude
import Test.Hspec
import Text.Pandoc.Definition (Inline (Str))
import Text.Pandoc.Util (PandocLink (..))
import Text.URI (mkURI)

spec :: Spec
spec = do
  -- The Markdown parser converts wiki-links to z: links, which should work.
  describe "z:/ links" $ do
    let zLink s = mkURILink s s
    it "parses custom/hash ID" $ do
      queryFromPandocLink (zLink "z:/foo-bar")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") Folgezettel)
    it "even with ?cf" $ do
      queryFromPandocLink (zLink "z:/foo-bar?cf")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
    it "parses prefixed short link" $ do
      queryFromPandocLink (zLink "z:/foo-bar")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") Folgezettel)
    it "resolves ambiguity using absolute URI" $ do
      queryFromPandocLink (zLink "z:/tags")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "tags") Folgezettel)
    it "z:zettels" $ do
      queryFromPandocLink (zLink "z:zettels")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] Folgezettel def)
    it "z:zettels?tag=foo" $ do
      queryFromPandocLink (zLink "z:zettels?tag=foo")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [mkTagPattern "foo"] Folgezettel def)
    it "z:zettels?cf" $ do
      queryFromPandocLink (zLink "z:zettels?cf")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelsByTag [] OrdinaryConnection def)
    it "z:tags" $ do
      queryFromPandocLink (zLink "z:tags")
        `shouldBe` (Just $ Some $ ZettelQuery_Tags [])
    it "z:tags?filter=foo" $ do
      queryFromPandocLink (zLink "z:tags?filter=foo")
        `shouldBe` (Just $ Some $ ZettelQuery_Tags [mkTagPattern "foo"])
    it "z:tag/foo" $ do
      queryFromPandocLink (zLink "z:tag/foo")
        `shouldBe` (Just $ Some $ ZettelQuery_TagZettel (Tag "foo"))
    it "z:tag/foo/bar/baz" $ do
      queryFromPandocLink (zLink "z:tag/foo/bar/baz")
        `shouldBe` (Just $ Some $ ZettelQuery_TagZettel (Tag "foo/bar/baz"))
    it "i18n" $ do
      let encodeUriPath = toText . E.encode
          zLinkUnicode s = mkURILink s s
      queryFromPandocLink (zLinkUnicode $ "z:/" <> encodeUriPath "计算机")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "计算机") Folgezettel)
  let normalLink = mkURILink "some link text"
  describe "flexible links (regular markdown)" $ do
    it "Default connection type should be cf" $ do
      queryFromPandocLink (normalLink "z:/foo-bar")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
    it "Supports full filename instead of zettel ID" $ do
      queryFromPandocLink (normalLink "foo-bar.md")
        `shouldBe` (Just $ Some $ ZettelQuery_ZettelByID (unsafeMkZettelID "foo-bar") OrdinaryConnection)
  describe "non-connection links" $ do
    it "pass through normal links" $ do
      queryFromPandocLink (normalLink "https://www.srid.ca")
        `shouldBe` Nothing
      queryFromPandocLink (normalLink "/static/resume.pdf")
        `shouldBe` Nothing
      queryFromPandocLink (normalLink "/static/")
        `shouldBe` Nothing
      queryFromPandocLink (normalLink "/static")
        `shouldBe` Nothing

mkURILink :: Text -> Text -> PandocLink
mkURILink linkText s =
  -- TODO: Do this in reflex-dom-pandoc
  let uri = either (error . toText . displayException) id $ mkURI s
      inner = if linkText == s then Nothing else Just [Str linkText]
   in PandocLink inner uri
