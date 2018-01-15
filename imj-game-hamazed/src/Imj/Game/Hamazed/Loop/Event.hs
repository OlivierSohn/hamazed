{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Event
    ( needsRendering
    , getEvent
    -- * Reexports
    , module Imj.Game.Hamazed.Loop.Event.Types
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Loop.Deadlines
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.State
import           Imj.Input.Types
import           Imj.Timing

-- | Tells if after handling an 'Event' we should render or not.
needsRendering :: Event -> Bool
needsRendering = \case
  (Action Ship _) -> False -- When the ship accelerates, nothing changes visually
  _ -> True

getEvent :: (MonadState AppState m, PlayerInput i, MonadReader i m, MonadIO m)
         => m Event
getEvent =
  getEvent' >>= \case
    Just event -> return event
    Nothing -> getEvent

getEvent' :: (MonadState AppState m, PlayerInput i, MonadReader i m, MonadIO m)
          => m (Maybe Event)
getEvent' = do
  t <- liftIO getSystemTime
  deadline <- getNextDeadline t
  getEventForMaybeDeadline deadline t
