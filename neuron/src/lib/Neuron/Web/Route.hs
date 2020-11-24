{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Neuron's route and its config
module Neuron.Web.Route where

import Control.Monad.Reader
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Some
import Data.TagTree (Tag)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Relude

data Route a where
  -- Route_Redirect :: Slug -> Route Slug
  -- ZIndex takes a report of all errors in the zettelkasten.
  -- `Left` is skipped zettels; and Right is valid zettels with invalid query links.
  Route_ZIndex :: Route (Map ZettelID (NonEmpty ZettelError))
  -- | Takes search JS code as render data
  -- The tag argument is only used in rendering the URL, and not when writing the file.
  -- TODO: Fix this bad use of types.
  Route_Search :: Maybe Tag -> Route Text
  Route_Zettel :: Slug -> Route ZettelC

data RouteConfig t m = RouteConfig
  { -- | Whether the view is being rendered for static HTML generation
    routeConfigStaticallyGenerated :: Bool,
    -- | How to render a web route.
    routeConfigRouteLink :: DomBuilder t m => Some Route -> Map Text Text -> m () -> m (),
    -- | Get the URL for a web route as plain text
    routeConfigRouteURL :: Some Route -> Text
  }

type NeuronWebT t m = ReaderT (RouteConfig t m) m

runNeuronWeb :: RouteConfig t m -> NeuronWebT t m a -> m a
runNeuronWeb = flip runReaderT

whenStaticallyGenerated :: Monad m => NeuronWebT t m () -> NeuronWebT t m ()
whenStaticallyGenerated f = do
  staticGen <- asks routeConfigStaticallyGenerated
  when staticGen f

neuronRouteLink :: DomBuilder t m => Some Route -> Map Text Text -> m () -> NeuronWebT t m ()
neuronRouteLink someR attrs w = do
  f <- asks routeConfigRouteLink
  lift $ f someR attrs w

neuronRouteURL :: Monad m => Some Route -> NeuronWebT t m Text
neuronRouteURL someR = do
  f <- asks routeConfigRouteURL
  pure $ f someR

routeTitle' :: a -> Route a -> Text
routeTitle' v = \case
  -- Route_Redirect _ -> "Redirecting..."
  Route_ZIndex -> "Zettel Index"
  Route_Search _mtag -> "Search"
  Route_Zettel _ ->
    either zettelTitle zettelTitle v

deriveGEq ''Route

deriveGShow ''Route
