{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.TagSpec
  ( spec,
  )
where

import qualified Neuron.Zettelkasten.Tag as Z
import Relude
import Test.Hspec

shouldMatch :: Z.TagPattern -> Z.Tag -> Expectation
shouldMatch pat tag
  | Z.tagMatch pat tag = pure ()
  | otherwise = expectationFailure $ toString $ Z.tagPatternToText pat <> " was expected to match " <> Z.tagToText tag <> " but didn't"

shouldNotMatch :: Z.TagPattern -> Z.Tag -> Expectation
shouldNotMatch pat tag
  | Z.tagMatch pat tag = expectationFailure $ toString $ Z.tagPatternToText pat <> " wasn't expected to match tag " <> Z.tagToText tag <> " but it did"
  | otherwise = pure ()

spec :: Spec
spec = do
  describe "Tag matching" $ do
    forM_ tagMatchCases $ \(name, Z.TagPattern -> pat, Z.Tag -> tag, expectedToMatch) -> do
      it name $ if expectedToMatch then pat `shouldMatch` tag else pat `shouldNotMatch` tag

tagMatchCases :: [(String, String, Text, Bool)]
tagMatchCases =
  [ ( "matches itself when the tag pattern is literal",
      "journal/note",
      "journal/note",
      True
    ),
    ( "matches recursive globs lazily",
      "math/**/note",
      "math/algebra",
      False
    ),
    ( "matches a single tag component on glob wildcard",
      "project/*/task",
      "project/neuron/hierarchical-tags/task",
      False
    ),
    ( "can match empty with globstar",
      "math/**/note",
      "math/note",
      True
    )
  ]
