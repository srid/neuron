{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Query
  ( runQuery,
  )
where

import Data.Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Map.Strict as Map
import Data.Some
import Data.Tree (Tree (..))
import Development.Shake (Action)
import qualified Neuron.Util.Tree as Z
import Neuron.Zettelkasten.ID (zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.Query as Z
import qualified Neuron.Zettelkasten.Store as Z
import Neuron.Zettelkasten.Tag (Tag (..), tagComponents)
import Neuron.Zettelkasten.Zettel (Zettel (..), zettelJson)
import Relude
import qualified Rib
import System.FilePath

runQuery :: FilePath -> Some Z.Query -> Action ()
runQuery notesDir query = do
  store <- Z.mkZettelStore =<< Rib.forEvery ["*.md"] pure
  case query of
    Some (Z.Query_ZettelByID zid) -> do
      let res = Z.lookupStore zid store
      putLTextLn $ Aeson.encodeToLazyText $ zettelJsonWith res
    Some (Z.Query_ZettelsByTag pats) -> do
      let res = Z.runQuery store (Z.Query_ZettelsByTag pats)
      putLTextLn $ Aeson.encodeToLazyText $ zettelJsonWith <$> res
    Some (Z.Query_Tags pats) -> do
      let tags = Z.runQuery store (Z.Query_Tags pats)
          tagPaths = fmap tagComponents $ Map.keys tags
          ann path =
            let tag = fold $ intersperse "/" path
             in fromMaybe 0 $ Map.lookup (Tag tag) tags
          tree = Z.annotatePaths ann <$> Z.mkTreeFromPaths tagPaths
      putLTextLn $ Aeson.encodeToLazyText $ toJSON $ fmap treeToJson tree
  where
    treeToJson (Node (tag, count) children) =
      object
        [ "tag" .= tag,
          "count" .= count,
          "children" .= fmap treeToJson children
        ]
    zettelJsonWith z@Zettel {..} =
      object $
        [ "path" .= (notesDir </> zettelIDSourceFileName zettelID)
        ]
          <> zettelJson z
