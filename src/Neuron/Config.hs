{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Config
  ( Config (..),
    getConfig,
    Alias (..),
    getAliases,
    aliasParser,
  )
where

import Control.Monad.Except
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import Development.Shake (Action, readFile')
import Dhall (FromDhall)
import qualified Dhall
import Dhall.TH
import Neuron.Parser
import qualified Neuron.Zettelkasten.ID as Z
import qualified Neuron.Zettelkasten.Store as Z
import Relude
import qualified Rib
import System.Directory
import System.FilePath
import qualified Text.Megaparsec.Char as M

-- | Config type for @neuron.dhall@
--
-- See <https://neuron.srid.ca/2011701.html guide> for description of the fields.
makeHaskellTypes
  [ SingleConstructor "Config" "Config" "./src-dhall/Config/Type.dhall"
  ]

deriving instance Generic Config

deriving instance FromDhall Config

data Alias = Alias
  { aliasZettel :: Z.ZettelID,
    targetZettel :: Z.ZettelID
  }
  deriving (Eq, Show)

defaultConfig :: ByteString
defaultConfig = $(embedFile "./src-dhall/Config/Default.dhall")

configFile :: FilePath
configFile = "neuron.dhall"

-- | Read the optional @neuron.dhall@ config file from the zettelksaten
getConfig :: Action Config
getConfig = do
  inputDir <- Rib.ribInputDir
  let configPath = inputDir </> configFile
  configVal :: Text <- liftIO (doesFileExist configPath) >>= \case
    True -> do
      userConfig <- fmap toText $ readFile' configPath
      -- Dhall's combine operator (`//`) allows us to merge two records,
      -- effectively merging the record with defaults with the user record.
      pure $ decodeUtf8 defaultConfig <> " // " <> userConfig
    False ->
      pure $ decodeUtf8 @Text defaultConfig
  parseConfig configVal

parseConfig :: MonadIO m => Text -> m Config
parseConfig s =
  liftIO $ Dhall.input Dhall.auto s

getAliases :: MonadFail m => Config -> Z.ZettelStore -> m [Alias]
getAliases Config {..} zettelStore = do
  let aliasSpecs = case aliases of
        -- In the absence of an index zettel, create an an alias to the z-index
        [] -> bool ["index:z-index"] [] $ hasIndexZettel zettelStore
        as -> as
  case mkAliases aliasSpecs zettelStore of
    Left err ->
      fail $ "Bad aliases in config: " <> toString err
    Right v ->
      pure v
  where
    hasIndexZettel =
      isJust . Map.lookup (Z.parseZettelID "index")

mkAliases :: [Text] -> Z.ZettelStore -> Either Text [Alias]
mkAliases aliasSpecs zettelStore =
  sequence $ flip fmap aliasSpecs $ \aliasSpec -> runExcept $ do
    alias@Alias {..} <- liftEither $ parse aliasParser configFile aliasSpec
    when (isJust $ Map.lookup aliasZettel zettelStore) $ do
      throwError $
        "Cannot create redirect from '" <> Z.zettelIDText aliasZettel <> "', because a zettel with that ID already exists"
    when (Z.zettelIDText targetZettel /= "z-index" && isNothing (Map.lookup targetZettel zettelStore)) $ do
      throwError $
        "Target zettel '" <> Z.zettelIDText targetZettel <> "' does not exist"
    pure alias

aliasParser :: Parser Alias
aliasParser =
  Alias <$> (Z.idParser <* M.char ':') <*> Z.idParser
