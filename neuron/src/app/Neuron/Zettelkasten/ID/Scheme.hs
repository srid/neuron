{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ID.Scheme
  ( nextAvailableZettelID,
    genVal,
    IDScheme (..),
    IDConflict (..),
  )
where

import Control.Monad.Except
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Neuron.Zettelkasten.ID
import Relude
import Text.Megaparsec.Simple
import Text.Show

-- | The scheme to use when generating new IDs
data IDScheme a where
  -- | Legacy date IDs (deprecated)
  IDSchemeDate :: Day -> IDScheme ()
  -- | Random IDs (default)
  IDSchemeHash :: IDScheme UUID
  -- | Custom ID (specified by the user)
  IDSchemeCustom :: Text -> IDScheme ()

data IDConflict
  = IDConflict_AlreadyExists
  | IDConflict_DateIDExhausted
  | IDConflict_HashConflict Text
  | IDConflict_BadCustomID Text Text
  deriving (Eq)

instance Show IDConflict where
  show = \case
    IDConflict_AlreadyExists ->
      "A zettel with that ID already exists"
    IDConflict_DateIDExhausted ->
      "Ran out of date ID indices for this day"
    IDConflict_HashConflict s ->
      "Hash conflict on " <> toString s <> "; try again"
    IDConflict_BadCustomID s e ->
      "The custom ID " <> toString s <> " is malformed: " <> toString e

-- | Produce a value that is required ahead to run an ID scheme.
genVal :: forall a. IDScheme a -> IO a
genVal = \case
  IDSchemeHash ->
    nextRandom
  IDSchemeDate _ ->
    pure ()
  IDSchemeCustom _ ->
    pure ()

-- | Create a new zettel ID based on the given scheme
--
-- This is a pure function, with all impure actions done in @genVal@
--
-- Ensures that new ID doesn't conflict with existing zettels.
nextAvailableZettelID ::
  forall a.
  -- Existing zettels
  Set ZettelID ->
  -- Seed value for the scheme
  a ->
  -- Scheme to use when generating an ID
  IDScheme a ->
  Either IDConflict ZettelID
nextAvailableZettelID zs val = \case
  IDSchemeDate day -> do
    let dayIndices = nonEmpty $
          sort $
            flip mapMaybe (Set.toList zs) $ \case
              ZettelDateID d x
                | d == day -> Just x
              _ -> Nothing
    case last <$> dayIndices of
      Nothing -> pure $ ZettelDateID day 1
      Just 99 -> throwError IDConflict_DateIDExhausted
      Just idx -> pure $ ZettelDateID day (idx + 1)
  IDSchemeHash -> do
    let s = T.take 8 $ UUID.toText val
    if s `Set.member` (zettelIDText `Set.map` zs)
      then throwError $ IDConflict_HashConflict s
      else
        either (error . toText) (pure . ZettelCustomID) $
          parse customIDParser "<random-hash>" s
  IDSchemeCustom s -> runExcept $ do
    zid <-
      either (throwError . IDConflict_BadCustomID s) (pure . ZettelCustomID) $
        parse customIDParser "<next-id>" s
    if zid `Set.member` zs
      then throwError IDConflict_AlreadyExists
      else pure zid

deriveGEq ''IDScheme

deriveGShow ''IDScheme
