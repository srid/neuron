{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Neuron's route and its config
module Neuron.Web.Route where

import Data.GADT.Compare.TH (DeriveGEQ (deriveGEq))
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Some (Some)
import Data.TagTree (Tag)
import Neuron.Web.Cache.Type (NeuronCache)
import Neuron.Zettelkasten.ID (Slug)
import Neuron.Zettelkasten.Zettel
  ( ZettelC,
    ZettelT (zettelTitle),
  )
import Reflex.Dom.Core (DomBuilder)
import Relude

-- TODO: Do we even need a route GADT? Re-evaluate this when doing the rib->reflex-headless migration.
data Route a where
  Route_Zettel :: Slug -> Route ZettelC
  -- | Impulse is implemented in github.com/srid/rememorate
  -- The tag argument is only used in rendering the URL, and not when writing the file.
  -- TODO: Fix this bad use of types.
  Route_Impulse :: Maybe Tag -> Route (NeuronCache, Text)

routeHtmlPath :: Route a -> FilePath
routeHtmlPath = \case
  Route_Impulse _mtag ->
    "impulse.html"
  Route_Zettel slug ->
    toString slug <> ".html"

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
  Route_Impulse _mtag -> "Impulse"
  Route_Zettel _ ->
    either zettelTitle zettelTitle v

deriveGEq ''Route

deriveGShow ''Route
