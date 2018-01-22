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
      , getGameParameters
      , getWorld
      , getUserIntent
      , getMode
      , getCurScreen
      , getLastRenderTime
      , hasVisibleNonRenderedUpdates
      , envFunctions
      -- * Modify
      , putGame
      , putGameState
      , putGameParameters
      , putWorld
      , putUserIntent
      , addParticleSystems
      , updateOneParticleSystem
      -- * reexports
      , MonadState
      ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State(get, put)
import           Data.Map.Strict(fromList, union, updateWithKey)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.ParticleSystem.Design.Update
import           Imj.Graphics.Text.ColorString

data Occurences a = Occurences {
    _occurencesCount :: !Int
  , _occurencesItem :: !EventRepr
}

data AppState  = AppState {
    _appStateTimeAfterRender :: !(Time Point System)
  , _appStateGame :: !Game
  , _appStateEvents :: !EventGroup
  , _appStateEventHistory :: !OccurencesHist
  -- ^ Can record which events where handled.
  , _appStateRecordEvents :: !RecordMode
  -- ^ Should the handled events be recorded?
  , _nextParticleSystemKey :: !ParticleSystemKey
  , _appStateDebug :: !Bool
  -- ^ Print times and group information in the terminal.
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
               | AnimateParticleSystem'
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
getGame = get >>= \(AppState _ g _ _ _ _ _) -> return g

{-# INLINABLE getGameState #-}
getGameState :: MonadState AppState m => m GameState
getGameState = getGame >>= \(Game _ _ s) -> return s

{-# INLINABLE getGameParameters #-}
getGameParameters :: MonadState AppState m => m GameParameters
getGameParameters = getGame >>= \(Game _ p _) -> return p

{-# INLINABLE getWorld #-}
getWorld :: MonadState AppState m => m World
getWorld = getGameState >>= \(GameState _ w _ _ _ _ _) -> return w

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
getLastRenderTime :: MonadState AppState m => m (Time Point System)
getLastRenderTime = get >>= \(AppState t _ _ _ _ _ _) -> return t

{-# INLINABLE putGame #-}
putGame :: MonadState AppState m => Game -> m ()
putGame g = get >>= \(AppState a _ e r h d f) ->
  put $ AppState a g e r h d f

{-# INLINABLE putGameState #-}
putGameState :: MonadState AppState m => GameState -> m ()
putGameState s = getGame >>= \(Game a b _) ->
  putGame $ Game a b s

{-# INLINABLE putGameParameters #-}
putGameParameters :: MonadState AppState m => GameParameters -> m ()
putGameParameters p = getGame >>= \(Game a _ b) ->
  putGame $ Game a p b

{-# INLINABLE putWorld #-}
putWorld :: MonadState AppState m => World -> m ()
putWorld w = getGameState >>= \(GameState a _ c d e f g) ->
  putGameState $ GameState a w c d e f g

{-# INLINABLE takeKeys #-}
takeKeys :: MonadState AppState m => Int -> m [ParticleSystemKey]
takeKeys n
  | n <= 0 = return []
  | otherwise =
      get >>= \(AppState a b c d e key f) -> do
        let endKey = key + fromIntegral n
        put $ AppState a b c d e endKey f
        return [key..pred endKey]

{-# INLINABLE addParticleSystems #-}
addParticleSystems :: MonadState AppState m
                   => [Prioritized ParticleSystem]
                   -> m ()
addParticleSystems l = do
  keys <- takeKeys $ length l
  let ps2 = fromList $ zip keys l
  getGameState >>= \(GameState a (World b c d ps) e f g h i) ->
    putGameState $ GameState a (World b c d (union ps ps2)) e f g h i

{-# INLINABLE updateOneParticleSystem #-}
updateOneParticleSystem :: MonadState AppState m
                        => ParticleSystemKey
                        -> Time Point System
                        -> m ()
updateOneParticleSystem key t =
  getWorld >>= \(World a b c systems) -> do
    let tps = systemTimePointToParticleSystemTimePoint t
        newSystems =
          updateWithKey
            (\_ (Prioritized p ps) -> fmap (Prioritized p) $ updateParticleSystem tps ps)
            key systems
    putWorld $ (World a b c newSystems)

{-# INLINABLE putUserIntent #-}
putUserIntent :: MonadState AppState m => UserIntent -> m ()
putUserIntent i =
  getGame >>= \(Game _ a b) -> putGame $ Game i a b

{-# INLINABLE hasVisibleNonRenderedUpdates #-}
hasVisibleNonRenderedUpdates :: MonadState AppState m => m Bool
hasVisibleNonRenderedUpdates =
  get >>= \(AppState _ _ group _ _ _ _) -> return $ visible group

-- | Creates environment functions taking into account a 'World' and 'Scope'
{-# INLINABLE envFunctions #-}
envFunctions :: (MonadState AppState m)
             => Scope -> m EnvFunctions
envFunctions scope = do
  world <- getWorld
  mode <- getMode
  screen <- getCurScreen
  return $ EnvFunctions (environmentInteraction world mode screen scope) envDistance
