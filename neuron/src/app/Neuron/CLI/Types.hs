{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Types
  ( -- * CLI
    App (..),

    -- * App monad
    AppT,
    getApp,
    runAppT,
    MonadApp (..),

    -- * CLI commands
    Command (..),
    NewCommand (..),
    SearchBy (..),
    SearchCommand (..),
    OpenCommand (..),
    QueryCommand (..),
    GenCommand (..),
  )
where

import Data.Some (Some (..))
import Data.Time.DateMayTime
  ( DateMayTime,
  )
import qualified Neuron.Frontend.Route as R
import Neuron.Zettelkasten.ID.Scheme (IDScheme (..))
import Neuron.Zettelkasten.Query.Graph as Q (GraphQuery (..))
import Neuron.Zettelkasten.Zettel as Q
  ( ZettelQuery (..),
  )
import Relude
import System.FilePath ((</>))

data App = App
  { notesDir :: FilePath,
    outputDir :: Maybe FilePath,
    cmd :: Command
  }
  deriving (Show)

newtype AppT m a = AppT (ReaderT App m a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans)

getApp :: Monad m => AppT m App
getApp = AppT ask

runAppT :: App -> AppT m a -> m a
runAppT appEnv (AppT m) =
  runReaderT m appEnv

class MonadApp m where
  getNotesDir :: m FilePath
  getOutputDir :: m FilePath

instance Monad m => MonadApp (AppT m) where
  getNotesDir =
    AppT $ reader notesDir
  getOutputDir = do
    mOut <- AppT $ reader outputDir
    case mOut of
      Nothing ->
        getNotesDir <&> (</> ".neuron" </> "output")
      Just fp ->
        pure fp

data NewCommand = NewCommand
  { date :: DateMayTime,
    idScheme :: Some IDScheme,
    edit :: Bool
  }
  deriving (Eq, Show)

data SearchCommand = SearchCommand
  { searchBy :: SearchBy,
    searchEdit :: Bool
  }
  deriving (Eq, Show)

data SearchBy
  = SearchByTitle
  | SearchByContent
  deriving (Eq, Show)

newtype OpenCommand = OpenCommand
  { unOpenCommand :: Some R.Route
  }
  deriving (Eq, Show)

data QueryCommand = QueryCommand
  { -- Use cache instead of building the zettelkasten from scratch
    cached :: Bool,
    query :: Either (Some Q.ZettelQuery) (Some Q.GraphQuery)
  }
  deriving (Eq, Show)

data GenCommand = GenCommand
  { -- | Whether to run a HTTP server on `outputDir`
    serve :: Maybe (Text, Int),
    watch :: Bool
  }
  deriving (Eq, Show)

data Command
  = -- | Create a new zettel file
    New NewCommand
  | -- | Open the locally generated Zettelkasten
    Open OpenCommand
  | -- | Search a zettel by title
    Search SearchCommand
  | -- | Run a query against the Zettelkasten
    Query QueryCommand
  | -- | Run site generation
    Gen GenCommand
  deriving (Eq, Show)
