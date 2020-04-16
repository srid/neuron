{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.TagSpec
  ( spec,
  )
where

import qualified Neuron.Zettelkasten.Tag as Z
import Relude
import Test.Hspec

shouldParse :: (Show a, Eq a) => Either Text a -> a -> Expectation
shouldParse (Right result) expected = result `shouldBe` expected
shouldParse (Left err) _ = expectationFailure (toString err)

shouldNotParse :: Show a => Either Text a -> Expectation
shouldNotParse result = result `shouldSatisfy` isLeft

shouldMatch :: Z.TagPattern -> Z.Tag -> Expectation
shouldMatch pat tag = Z.tagMatch pat tag `shouldBe` True

shouldNotMatch :: Z.TagPattern -> Z.Tag -> Expectation
shouldNotMatch pat tag = Z.tagMatch pat tag `shouldBe` False

spec :: Spec
spec = do
  describe "Tags" $ do
    context "tag parsing" $ do
      it "parses a simple tag" $ do
        Z.parseTag "science" `shouldParse` Z.Tag ["science"]
      it "parses a hierachical tag" $ do
        Z.parseTag "journal/work" `shouldParse` Z.Tag ["journal", "work"]
      it "handles non ASCII chars" $ do
        Z.parseTag "mathématiques" `shouldParse` Z.Tag ["mathématiques"]
      it "can contain hyphens" $ do
        Z.parseTag "math/linear-algebra" `shouldParse` Z.Tag ["math", "linear-algebra"]
      it "fails when tag contains spaces" $ do
        shouldNotParse $ Z.parseTag "math/linear algebra"
      it "cannot contain stars" $ do
        shouldNotParse $ Z.parseTag "algorithms/a*"
      it "parses literal pattern" $ do
        Z.parseTagPattern "journal/diet" `shouldParse` Z.TagPattern [Z.Literal "journal", Z.Literal "diet"]
      it "parses recursive glob" $ do
        Z.parseTagPattern "math/**/note" `shouldParse` Z.TagPattern [Z.Literal "math", Z.GlobStar, Z.Literal "note"]
    context "tag pattern" $ do
      let mathTags =
            [ Z.Tag ["math", "calculus"],
              Z.Tag ["math", "topology"],
              Z.Tag ["math", "algebra"],
              Z.Tag ["math", "algebra", "linear"],
              Z.Tag ["math", "algebra", "homological"]
            ] ::
              [Z.Tag]
          journalNoteTag = Z.Tag ["journal", "note"]
          courseNoteTags =
            [ Z.Tag ["course", "math", "note"],
              Z.Tag ["course", "sociology", "note"]
            ] ::
              [Z.Tag]
          noteTags = journalNoteTag : courseNoteTags
      it "matches itself when is a literal pattern" $ do
        forM_ mathTags $ \tag -> Z.tagLiteral tag `shouldMatch` tag
      it "handles infix globs correctly" $ do
        let pat = Z.TagPattern [Z.Literal "course", Z.Glob, Z.Literal "note"]
        shouldMatch pat `mapM_` courseNoteTags
        shouldNotMatch pat journalNoteTag
      context "recursive glob" $ do
        it "fails to match end of tag" $ do
          Z.TagPattern [Z.Literal "math", Z.GlobStar] `shouldNotMatch` Z.Tag ["math"]
        it "matches eagerly when terminal" $ do
          shouldMatch (Z.TagPattern [Z.Literal "math", Z.GlobStar]) `mapM_` mathTags
        it "matches lazily when non terminal" $ do
          shouldMatch (Z.TagPattern [Z.GlobStar, Z.Literal "note"]) `mapM_` noteTags
