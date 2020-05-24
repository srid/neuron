{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph.Build where

import Control.Monad.Writer (runWriterT)
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error (QueryParseError)
import Neuron.Zettelkasten.Query.Eval (queryConnections)
import Neuron.Zettelkasten.Zettel
import Relude

-- | Build the Zettelkasten graph from a list of zettels
--
-- If there are any errors during parsing of queries (to determine connections),
-- return them as well.
mkZettelGraph ::
  [PandocZettel] ->
  ( ZettelGraph,
    Map ZettelID [QueryParseError]
  )
mkZettelGraph zettels =
  let res :: [(Zettel, ([(Maybe Connection, Zettel)], [QueryParseError]))] =
        flip runReader (fmap (fst . unPandocZettel) zettels) $ do
          for zettels $ \(PandocZettel (z, body)) -> fmap (z,) $ do
            runWriterT $ queryConnections body
      g :: ZettelGraph = G.mkGraphFrom (fst <$> res) $ flip concatMap res $ \(z1, fst -> conns) ->
        conns <&> \(c, z2) -> (connectionMonoid (fromMaybe Folgezettel c), z1, z2)
   in ( g,
        Map.fromList $ flip mapMaybe res $ \(z, (_conns, errs)) ->
          if null errs
            then Nothing
            else Just (zettelID z, errs)
      )
  where
    connectionMonoid = Just
