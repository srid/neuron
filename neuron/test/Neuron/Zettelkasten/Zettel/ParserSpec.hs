{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.ParserSpec
  ( spec,
  )
where

import Data.TagTree (Tag (Tag))
import Neuron.Reader.Markdown (parseMarkdown)
import Neuron.Reader.Type (ZettelFormat (ZettelFormat_Markdown))
import Neuron.Zettelkasten.ID (ZettelID (ZettelID))
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelT (zettelTags),
    sansContent,
  )
import Neuron.Zettelkasten.Zettel.Parser (extractQueriesWithContext, parseZettel)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "inline tags" $ do
    let parseSomeZettel =
          sansContent
            . parseZettel
              ZettelFormat_Markdown
              parseMarkdown
              extractQueriesWithContext
              "<test>"
              (ZettelID "test")
    it "simple" $ do
      let z :: Zettel = parseSomeZettel "An #inline tag"
      zettelTags z `shouldBe` [Tag "inline"]
    it "hierarchical" $ do
      let z :: Zettel = parseSomeZettel "An #foo/bar/baz tag"
      zettelTags z `shouldBe` [Tag "foo/bar/baz"]
    it "followed by punctuation" $ do
      let z :: Zettel = parseSomeZettel "A #tag; with content"
      zettelTags z `shouldBe` [Tag "tag"]
    it "followed by different punctuation" $ do
      let z :: Zettel = parseSomeZettel "A #tag? With content"
      zettelTags z `shouldBe` [Tag "tag"]
    it "allows URLs with a hash" $ do
      pendingWith "#397"
      let z :: Zettel = parseSomeZettel "Some http://www.google.com/#foo url"
      zettelTags z `shouldBe` []