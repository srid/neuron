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

spec :: Spec
spec = do
  describe "Tag matching" $ do
    forM_ tagMatchCases $ \(name, Z.mkTagPattern . toText -> pat, fmap Z.Tag -> matching, fmap Z.Tag -> failing) -> do
      it name $ do
        forM_ matching $ \tag -> do
          pat `shouldMatch` tag
        forM_ failing $ \tag -> do
          pat `shouldNotMatch` tag

tagMatchCases :: [(String, String, [Text], [Text])]
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

shouldMatch :: Z.TagPattern -> Z.Tag -> Expectation
shouldMatch pat tag
  | Z.tagMatch pat tag = pure ()
  | otherwise =
    expectationFailure $
      Z.unTagPattern pat <> " was expected to match " <> toString (Z.unTag tag) <> " but didn't"

shouldNotMatch :: Z.TagPattern -> Z.Tag -> Expectation
shouldNotMatch pat tag
  | Z.tagMatch pat tag =
    expectationFailure $
      Z.unTagPattern pat <> " wasn't expected to match tag " <> toString (Z.unTag tag) <> " but it did"
  | otherwise = pure ()
