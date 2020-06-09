{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Neuron's route and its config
module Neuron.Web.Route where

import Control.Monad.Reader
import Data.Some
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Relude

data Route a where
  Route_Redirect :: ZettelID -> Route ZettelID
  -- ZIndex takes a report of all errors in the zettelkasten.
  -- `Left` is skipped zettels; and Right is valid zettels with invalid query links.
  Route_ZIndex :: Route (Map ZettelID ZettelError)
  Route_Search :: Route ()
  Route_Zettel :: ZettelID -> Route ZettelC

type family RouteError r

type instance RouteError (Map ZettelID ZettelError) = ()

type instance RouteError ZettelID = ()

type instance RouteError () = ()

type instance RouteError ZettelC = [QueryError]

data RouteConfig t m = RouteConfig
  { -- | Whether the view is being rendered for static HTML generation
    routeConfigStaticallyGenerated :: Bool,
    -- | How to render a web route.
    routeConfigRouteLink :: DomBuilder t m => Some Route -> Map Text Text -> m () -> m ()
  }

type NeuronWebT t m = ReaderT (RouteConfig t m) m

runNeuronWeb :: RouteConfig t m -> NeuronWebT t m a -> m a
runNeuronWeb cfg = flip runReaderT cfg

whenStaticallyGenerated :: Monad m => NeuronWebT t m () -> NeuronWebT t m ()
whenStaticallyGenerated f = do
  staticGen <- asks routeConfigStaticallyGenerated
  when staticGen f

neuronRouteLink :: DomBuilder t m => Some Route -> Map Text Text -> m () -> NeuronWebT t m ()
neuronRouteLink someR attrs w = do
  f <- asks routeConfigRouteLink
  lift $ f someR attrs w

routeTitle' :: a -> Route a -> Text
routeTitle' v = \case
  Route_Redirect _ -> "Redirecting..."
  Route_ZIndex -> "Zettel Index"
  Route_Search -> "Search"
  Route_Zettel _ ->
    either zettelTitle zettelTitle v
