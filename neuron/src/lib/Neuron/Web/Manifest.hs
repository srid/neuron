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

mkManifest :: [FilePath] -> Manifest
mkManifest (Set.fromList -> files) =
  Manifest
    { manifestFavicons = mkFavicons,
      manifestWebAppManifest = lookupSet webmanifestFile files
    }
  where
    mkFavicons =
      case filter (`Set.member` files) favicons of
        (ico : alts) ->
          Just $
            Favicons
              { faviconsDefault = ico,
                faviconsAlts = alts,
                faviconsAppleTouch = lookupSet appleTouchIcon files
              }
        [] ->
          Nothing
    lookupSet x s =
      if Set.member x s
        then Just x
        else Nothing

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

manifestPatterns :: [FilePath]
manifestPatterns =
  [faviconsPattern, appleTouchIcon, webmanifestFile]

-- | NOTE: This pattern should cover all of `favicons`
faviconsPattern :: FilePath
faviconsPattern = "static/favicon.*"

-- | Supported favicons, in order of preference
favicons :: [FilePath]
favicons = fmap ("static/favicon." <>) ["svg", "png", "ico", "jpg", "jpeg"]

appleTouchIcon :: FilePath
appleTouchIcon = "static/apple-touch-icon.png"

webmanifestFile :: FilePath
webmanifestFile = "static/manifest.webmanifest"
