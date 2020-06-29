{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Manifest
  ( Manifest,
    manifestPatterns,
    mkManifest,
    renderManifest,
  )
where

import qualified Data.Set as Set
import Reflex.Dom.Core
import Relude

data Manifest = Manifest
  { manifestFavicons :: Maybe Favicons,
    manifestWebAppManifest :: Maybe FilePath
  }
  deriving (Eq)

data Favicons = Favicons
  { faviconsDefault :: FilePath,
    faviconsAlts :: [FilePath],
    faviconsAppleTouch :: Maybe FilePath
  }
  deriving (Eq)

manifestPatterns :: [FilePath]
manifestPatterns =
  ["static/favicon.*", "static/manifest.webmanifest"]

mkManifest :: [FilePath] -> Manifest
mkManifest (Set.fromList -> files) =
  Manifest
    { manifestFavicons = mkFavicons,
      manifestWebAppManifest = Nothing
    }
  where
    mkFavicons =
      case filter (`Set.member` files) favicons of
        (defIcon : alts) ->
          Just $
            Favicons
              { faviconsDefault = defIcon,
                faviconsAlts = alts,
                faviconsAppleTouch = bool Nothing (Just appleTouchIcon) $ Set.member appleTouchIcon files
              }
        [] ->
          Nothing
    -- Supported favicons, in order of preference
    favicons :: [FilePath]
    favicons = fmap ("static/favicon." <>) ["svg", "png", "ico"]
    appleTouchIcon :: FilePath
    appleTouchIcon = "static/apple-touch-icon.png"

renderManifest :: DomBuilder t m => Manifest -> m ()
renderManifest Manifest {..} = do
  linkRel "manifest" `mapM_` manifestWebAppManifest
  case manifestFavicons of
    Nothing ->
      linkRel "icon" defaultFaviconUrl
    Just Favicons {..} -> do
      linkRel "icon" faviconsDefault
      linkRel "alternate icon" `mapM_` faviconsAlts
      linkRel "apple-touch-icon" `mapM_` faviconsAppleTouch
  where
    defaultFaviconUrl :: String
    defaultFaviconUrl =
      "https://raw.githubusercontent.com/srid/neuron/master/assets/neuron.svg"
    linkRel rel path =
      elAttr "link" ("rel" =: rel <> "href" =: toText path) blank
