{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Manifest
  ( Manifest,
    manifestPatterns,
    mkManifest,
    mkManifestFromTree,
    renderManifest,
  )
where

import qualified Data.Set as Set
import Reflex.Dom.Core
import Relude
import qualified System.Directory.Contents as DC

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

mkManifestFromTree :: DC.DirTree FilePath -> Manifest
mkManifestFromTree fileTree =
  let availableFiles = fforMaybe manifestPatterns $ \fp -> do
        case DC.walkContents fp fileTree of
          Just (DC.DirTree_File _ _) ->
            Just fp
          _ ->
            Nothing
   in mkManifest availableFiles

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
      -- crossorigin="use-credentials"
      elAttr
        "link"
        ( "rel" =: rel
            <> "href" =: toText path
            -- The use-credentials value must be used when fetching a manifest that requires credentials, even if the file is from the same origin.
            -- cf. https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/crossorigin
            -- cf. https://thatemil.com/blog/2018/02/21/pwa-basic-auth/
            <> (if rel == "manifest" then "crossorigin" =: "use-credentials" else mempty)
        )
        blank

manifestPatterns :: [FilePath]
manifestPatterns =
  favicons <> [appleTouchIcon, webmanifestFile]

-- | Supported favicons, in order of preference
favicons :: [FilePath]
favicons = fmap ("static/favicon." <>) ["svg", "png", "ico", "jpg", "jpeg"]

appleTouchIcon :: FilePath
appleTouchIcon = "static/apple-touch-icon.png"

webmanifestFile :: FilePath
webmanifestFile = "static/manifest.webmanifest"
