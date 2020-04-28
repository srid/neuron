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
    BaseUrlError (..),
    getAliases,
    aliasParser,
    getMarkdownExtensions,
  )
where

import Control.Monad.Except
import Data.FileEmbed (embedFile)
import qualified Data.Graph.Labelled as G
import Development.Shake (Action, readFile')
import Dhall (FromDhall)
import qualified Dhall
import Dhall.TH
import Neuron.Zettelkasten.Graph.Type
import qualified Neuron.Zettelkasten.ID as Z
import Relude
import qualified Rib
import System.Directory
import System.FilePath
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension.Common as Ext
import Text.MMark.Extension.SetTableClass (setTableClass)
import qualified Text.Megaparsec.Char as M
import Text.Megaparsec.Simple

-- | Config type for @neuron.dhall@
--
-- See <https://neuron.zettel.page/2011701.html guide> for description of the fields.
makeHaskellTypes
  [ SingleConstructor "Config" "Config" "./src-dhall/Config/Type.dhall"
  ]

deriving instance Generic Config

deriving instance FromDhall Config

data BaseUrlError
  = BaseUrlNotAbsolute
  deriving (Eq, Show)

instance Exception BaseUrlError

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

getMarkdownExtensions :: Config -> [MMark.Extension]
getMarkdownExtensions Config {..} =
  defaultExts
    <> bool [] [Ext.mathJax (Just '$')] mathJaxSupport
  where
    defaultExts :: [MMark.Extension]
    defaultExts =
      [ Ext.fontAwesome,
        Ext.footnotes,
        Ext.kbd,
        Ext.linkTarget,
        Ext.punctuationPrettifier,
        Ext.skylighting,
        setTableClass "ui celled table"
      ]

getAliases :: MonadFail m => Config -> ZettelGraph -> m [Alias]
getAliases Config {..} graph = do
  let aliasSpecs = case aliases of
        -- In the absence of an index zettel, create an an alias to the z-index
        [] -> bool ["index:z-index"] [] $ hasIndexZettel graph
        as -> as
  case mkAliases aliasSpecs graph of
    Left err ->
      fail $ "Bad aliases in config: " <> toString err
    Right v ->
      pure v
  where
    hasIndexZettel =
      isJust . G.findVertex (Z.parseZettelID "index")

mkAliases :: [Text] -> ZettelGraph -> Either Text [Alias]
mkAliases aliasSpecs graph =
  sequence $ flip fmap aliasSpecs $ \aliasSpec -> runExcept $ do
    alias@Alias {..} <- liftEither $ parse aliasParser configFile aliasSpec
    when (isJust $ G.findVertex aliasZettel graph) $ do
      throwError $
        "Cannot create redirect from '" <> Z.zettelIDText aliasZettel <> "', because a zettel with that ID already exists"
    when (Z.zettelIDText targetZettel /= "z-index" && isNothing (G.findVertex targetZettel graph)) $ do
      throwError $
        "Target zettel '" <> Z.zettelIDText targetZettel <> "' does not exist"
    pure alias

aliasParser :: Parser Alias
aliasParser =
  Alias <$> (Z.idParser <* M.char ':') <*> Z.idParser
