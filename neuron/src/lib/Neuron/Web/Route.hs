{-# LANGUAGE GADTs #-}
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
  Route_ZIndex :: Route (Map ZettelID (Either Text [QueryError]))
  Route_Search :: Route ()
  Route_Zettel :: ZettelID -> Route PandocZettel

type family RouteError r

type instance RouteError (Map ZettelID (Either Text [QueryError])) = ()

type instance RouteError ZettelID = ()

type instance RouteError () = ()

type instance RouteError PandocZettel = [QueryError]

data RouteConfig t m = RouteConfig
  { -- | Whether the view is being rendered for static HTML generation
    routeConfigStaticallyGenerated :: Bool,
    -- | How to render a web route.
    routeConfigRouteLink :: DomBuilder t m => Some Route -> m () -> m ()
  }

type NeuronWebT t m = ReaderT (RouteConfig t m) m

runNeuronWeb :: RouteConfig t m -> NeuronWebT t m a -> m a
runNeuronWeb cfg = flip runReaderT cfg

neuronRouteLink :: DomBuilder t m => Some Route -> m () -> NeuronWebT t m ()
neuronRouteLink someR w = do
  f <- asks routeConfigRouteLink
  lift $ f someR w
