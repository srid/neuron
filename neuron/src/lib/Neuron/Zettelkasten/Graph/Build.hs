{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph.Build where

import Control.Monad.Writer (runWriterT)
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error (QueryParseError)
import Neuron.Zettelkasten.Query.Eval (queryConnections)
import Neuron.Zettelkasten.Zettel
import Relude
import Text.Pandoc.Definition (Pandoc)

-- | Build the Zettelkasten graph from a list of zettels
--
-- If there are any errors during parsing of queries (to determine connections),
-- return them as well.
mkZettelGraph ::
  [PandocZettel] ->
  ( ZettelGraph,
    Map ZettelID [QueryParseError]
  )
mkZettelGraph pZettels =
  let zettels = fmap (fst . unPandocZettel) pZettels
      res :: [(Zettel, ([(Maybe Connection, Zettel)], [QueryParseError]))] =
        flip fmap pZettels $ \(PandocZettel (z, doc)) ->
          (z, runQueryConnections zettels doc)
      g :: ZettelGraph = G.mkGraphFrom zettels $ flip concatMap res $ \(z1, fst -> conns) ->
        edgeFromConnection z1 <$> conns
      errors = Map.fromList $ flip mapMaybe res $ \(z, (snd -> errs)) ->
        if null errs
          then Nothing
          else Just (zettelID z, errs)
   in (g, errors)

runQueryConnections :: [Zettel] -> Pandoc -> ([(Maybe Connection, Zettel)], [QueryParseError])
runQueryConnections zettels doc =
  flip runReader zettels $ do
    runWriterT $ queryConnections doc

edgeFromConnection :: Zettel -> (Maybe Connection, Zettel) -> (Maybe Connection, Zettel, Zettel)
edgeFromConnection z (c, z2) =
  (connectionMonoid $ fromMaybe Folgezettel c, z, z2)
  where
    -- Our connection monoid will never be Nothing (mempty); see the note in
    -- type `ZettelGraph`.
    connectionMonoid = Just
