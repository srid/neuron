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

shouldMatch :: Z.TagPattern -> Z.Tag -> Expectation
shouldMatch pat tag = Z.tagMatch pat tag `shouldBe` True

shouldNotMatch :: Z.TagPattern -> Z.Tag -> Expectation
shouldNotMatch pat tag = Z.tagMatch pat tag `shouldBe` False

spec :: Spec
spec = do
  describe "Tags" $ do
    let mathTags =
          [ Z.Tag "math/calculus",
            Z.Tag "math/topology",
            Z.Tag "math/algebra",
            Z.Tag "math/algebra/linear",
            Z.Tag "math/algebra/homological"
          ] ::
            [Z.Tag]
        journalNoteTag = Z.Tag "journal/note"
        courseNoteTags =
          [ Z.Tag "course/math/note",
            Z.Tag "course/sociology/note"
          ] ::
            [Z.Tag]
        noteTags = journalNoteTag : courseNoteTags
    it "matches itself when is a literal pattern" $ do
      forM_ mathTags $ \tag -> Z.literalPattern tag `shouldMatch` tag
    it "handles infix globs correctly" $ do
      let pat = Z.TagPattern "course/*/note"
      shouldMatch pat `mapM_` courseNoteTags
      shouldNotMatch pat journalNoteTag
    context "recursive glob" $ do
      it "matches end of tag" $ do
        Z.TagPattern "math/**" `shouldMatch` Z.Tag "math"
      it "matches eagerly when terminal" $ do
        shouldMatch (Z.TagPattern "math/**") `mapM_` mathTags
      it "matches lazily when non terminal" $ do
        shouldMatch (Z.TagPattern "**/note") `mapM_` noteTags
