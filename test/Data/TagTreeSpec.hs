{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.TagTreeSpec
  ( spec,
  )
where

import Data.TagTree
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Tag matching" $ do
    forM_ tagMatchCases $ \(name, mkTagPattern -> pat, fmap Tag -> matching, fmap Tag -> failing) -> do
      it name $ do
        forM_ matching $ \tag -> do
          pat `shouldMatch` tag
        forM_ failing $ \tag -> do
          pat `shouldNotMatch` tag

tagMatchCases :: [(String, Text, [Text], [Text])]
tagMatchCases =
  [ ( "simple tag",
      "journal",
      ["journal"],
      ["science", "journal/work"]
    ),
    ( "simple tag with slash",
      "journal/note",
      ["journal/note"],
      ["science/physics", "journal", "journal/note/foo"]
    ),
    ( "tag pattern with **",
      "journal/**",
      ["journal", "journal/work", "journal/work/clientA"],
      ["math", "science/physics", "jour"]
    ),
    ( "tag pattern with */**",
      "journal/*/**",
      ["journal/foo", "journal/foo/bar"],
      ["science", "journal"]
    ),
    ( "tag pattern with ** in the middle",
      "math/**/note",
      ["math/note", "math/algebra/note", "math/algebra/linear/note"],
      ["math/algebra", "journal/note"]
    ),
    ( "tag pattern with * in the middle",
      "project/*/task",
      ["project/foo/task", "project/bar-baz/task"],
      ["project", "project/foo", "project/task", "project/foo/bar/task"]
    )
  ]

shouldMatch :: TagPattern -> Tag -> Expectation
shouldMatch pat tag
  | tagMatch pat tag = pure ()
  | otherwise =
    expectationFailure $
      unTagPattern pat <> " was expected to match " <> toString (unTag tag) <> " but didn't"

shouldNotMatch :: TagPattern -> Tag -> Expectation
shouldNotMatch pat tag
  | tagMatch pat tag =
    expectationFailure $
      unTagPattern pat <> " wasn't expected to match tag " <> toString (unTag tag) <> " but it did"
  | otherwise = pure ()
