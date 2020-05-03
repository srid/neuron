{-# LANGUAGE NoImplicitPrelude #-}

module Data.PathTreeSpec
  ( spec,
  )
where

import Data.Tree (Forest, Tree (..))
import Data.PathTree
import Relude
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "Path tree" $ do
    context "Tree building" $ do
      forM_ treeCases $ \(name, paths, tree) -> do
        it name $ do
          mkTreeFromPaths paths `shouldBe` tree
    context "Tree folding" $ do
      forM_ foldingCases $ \(name, tree, folded) -> do
        it name $ do
          let mergePaths (p, n) (p', b') = bool Nothing (Just (p </> p', b')) n
              res = fst <$> foldSingleParentsWith mergePaths tree
          res `shouldBe` folded

treeCases :: [(String, [[String]], Forest String)]
treeCases =
  [ ( "works on one level",
      [["journal"], ["science"]],
      [Node "journal" [], Node "science" []]
    ),
    ( "groups paths with common prefix",
      [["math", "algebra"], ["math", "calculus"]],
      [Node "math" [Node "algebra" [], Node "calculus" []]]
    ),
    ( "ignores tag when there is also tag/subtag",
      [["math"], ["math", "algebra"]],
      [Node "math" [Node "algebra" []]]
    )
  ]

foldingCases :: [(String, Tree (String, Bool), Tree String)]
foldingCases =
  [ ( "folds tree on one level",
      Node ("math", True) [Node ("note", False) []],
      Node "math/note" []
    ),
    ( "folds across multiple levels",
      Node ("math", True) [Node ("algebra", True) [Node ("note", False) []]],
      Node "math/algebra/note" []
    ),
    ( "does not fold tree when the predicate is false",
      Node ("math", False) [Node ("note", False) []],
      Node "math" [Node "note" []]
    )
  ]
