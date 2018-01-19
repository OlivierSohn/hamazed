{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.State.Types
      (
      -- * AppState type
        AppState(..)
      , RecordMode(..)
      , OccurencesHist(..)
      , Occurences(..)
      , EventRepr(..)
      -- * Access
      , getGame
      , getGameState
      , getUserIntent
      , getMode
      , getCurScreen
      , getLastRenderTime
      , hasVisibleNonRenderedUpdates
      , envFunctions
      -- * Modify
      , putGame
      , putUserIntent
      -- * reexports
      , MonadState
      , TimeSpec
      ) where

import           Imj.Prelude

import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State(get, put)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.Text.ColorString
import           Imj.Timing

data Occurences a = Occurences {
    _occurencesCount :: !Int
  , _occurencesItem :: !EventRepr
}

data AppState  = AppState {
    _appStateTimeAfterRender :: !TimeSpec
  , _appStateGame :: !Game
  , _appStateEvents :: !EventGroup
  , _appStateEventHistory :: !OccurencesHist
  -- ^ Can record which events where handled.
  , _appStateRecordEvents :: !RecordMode
  -- ^ Should the handled events be recorded?
}

data RecordMode = Record
                | DontRecord
                deriving(Eq)

data OccurencesHist = OccurencesHist {
    _occurencesHistList :: ![Occurences EventRepr]
  , _occurencesHistTailStr :: !ColorString
}

data EventRepr = Laser'
               | Ship'
               | MoveFlyingItems'
               | AnimateParticleSystems'
               | DisplayContinueMessage'
               | AnimateUI'
               | StartLevel'
               | Configuration'
               | StartGame'
               | EndGame'
               | Interrupt'
               | ToggleEventRecording'
               | IgnoredOverdue
               -- ^ Represents when an overdue deadline was ignored because its priority was lower
               -- than another overdue deadline.
               deriving(Eq, Show)


{-# INLINABLE getGame #-}
getGame :: MonadState AppState m => m Game
getGame =
  get >>= \(AppState _ g _ _ _) -> return g

{-# INLINABLE getGameState #-}
getGameState :: MonadState AppState m => m GameState
getGameState =
  getGame >>= \(Game _ _ s) -> return s

{-# INLINABLE getMode #-}
getMode :: MonadState AppState m => m ViewMode
getMode =
  getGame >>= \(Game _ (GameParameters _ _ mode) _) -> return mode

{-# INLINABLE getUserIntent #-}
getUserIntent :: MonadState AppState m => m UserIntent
getUserIntent =
  getGame >>= \(Game i _ _) -> return i

{-# INLINABLE getCurScreen #-}
getCurScreen :: MonadState AppState m => m Screen
getCurScreen =
  getGame >>= \(Game _ _ (GameState _ _ _ _ _ _ screen)) -> return screen

{-# INLINABLE getLastRenderTime #-}
getLastRenderTime :: MonadState AppState m => m TimeSpec
getLastRenderTime =
  get >>= \(AppState t _ _ _ _) -> return t

{-# INLINABLE putGame #-}
putGame :: MonadState AppState m => Game -> m ()
putGame g =
  get >>= \(AppState a _ e r h) ->
    put $ AppState a g e r h

putUserIntent :: MonadState AppState m => UserIntent -> m ()
putUserIntent i =
  getGame >>= \(Game _ a b) -> putGame $ Game i a b

{-# INLINABLE hasVisibleNonRenderedUpdates #-}
hasVisibleNonRenderedUpdates :: MonadState AppState m => m Bool
hasVisibleNonRenderedUpdates =
  get >>= \(AppState _ _ group _ _) -> return $ visible group

-- | Creates environment functions taking into account a 'World' and 'Scope'
{-# INLINABLE envFunctions #-}
envFunctions :: (MonadState AppState m)
             => World -> Scope -> m EnvFunctions
envFunctions world scope = do
  mode <- getMode
  screen <- getCurScreen
  return $ EnvFunctions (environmentInteraction world mode screen scope) envDistance
