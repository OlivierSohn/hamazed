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
      , getMyShipId
      , getGameConnection
      , getViewMode
      , getWorld
      , getClientState
      , getCurScreen
      , getLastRenderTime
      , hasVisibleNonRenderedUpdates
      , envFunctions
      -- * Modify
      , putGame
      , putGameState
      , putViewMode
      , putGameConnection
      , putWorld
      , putAnimation
      , putClientState
      , stateChat
      , addParticleSystems
      , updateOneParticleSystem
      -- * reexports
      , MonadState
      ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State.Strict(get, put)
import           Data.Map.Strict(fromList, union, updateWithKey)

import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
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
  , _appStateDebug :: {-# UNPACK #-} !Bool
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
               | PeriodicMotion'
               | MoveFlyingItems'
               | AnimateParticleSystem'
               | DisplayContinueMessage'
               | AnimateUI'
               | WorldRequest'
               | ChangeLevel'
               | Configuration'
               | CycleRenderingOptions'
               | StartGame'
               | EndLevel'
               | Interrupt'
               | ToggleEventRecording'
               | ConnectionAccepted'
               | Disconnected'
               | ConnectionRefused'
               | EnterState'
               | ExitState'
               | Chat'
               | Error'
               | IgnoredOverdue
               -- ^ Represents when an overdue deadline was ignored because its priority was lower
               -- than another overdue deadline.
               deriving(Eq, Show)


{-# INLINABLE getGame #-}
getGame :: MonadState AppState m => m Game
getGame = get >>= \(AppState _ g _ _ _ _ _) -> return g

{-# INLINABLE getGameState #-}
getGameState :: MonadState AppState m => m GameState
getGameState = getGameState' <$> getGame

{-# INLINABLE getViewMode #-}
getViewMode :: MonadState AppState m => m ViewMode
getViewMode = getViewMode' <$> getGame

{-# INLINABLE getWorld #-}
getWorld :: MonadState AppState m => m World
getWorld = getGameState >>= return . getPreviousWorld

{-# INLINABLE getClientState #-}
getClientState :: MonadState AppState m => m ClientState
getClientState = getClientState' <$> getGame

{-# INLINABLE getCurScreen #-}
getCurScreen :: MonadState AppState m => m Screen
getCurScreen =
  getGameState >>= return . getScreen

{-# INLINABLE getLastRenderTime #-}
getLastRenderTime :: MonadState AppState m => m (Time Point System)
getLastRenderTime = get >>= \(AppState t _ _ _ _ _ _) -> return t

{-# INLINABLE putGame #-}
putGame :: MonadState AppState m => Game -> m ()
putGame g = get >>= \(AppState a _ e r h d f) ->
  put $ AppState a g e r h d f

{-# INLINABLE putGameState #-}
putGameState :: MonadState AppState m => GameState -> m ()
putGameState s =
  getGame >>= \g -> putGame $ g {getGameState' = s}

{-# INLINABLE putAnimation #-}
putAnimation :: MonadState AppState m => UIAnimation -> m ()
putAnimation a =
  getGameState >>= \s -> putGameState $ s {getUIAnimation = a}

{-# INLINABLE putGameConnection #-}
putGameConnection :: MonadState AppState m => ConnectionStatus -> m ()
putGameConnection c =
  getGame >>= \g -> putGame $ g {connection = c}

{-# INLINABLE getGameConnection #-}
getGameConnection :: MonadState AppState m => m ConnectionStatus
getGameConnection =
  getGame >>= return . connection

{-# INLINABLE getMyShipId #-}
getMyShipId :: MonadState AppState m => m (Maybe ShipId)
getMyShipId =
  (\case
    Connected myId -> Just $ getClientId myId
    _ -> Nothing) <$> getGameConnection

{-# INLINABLE putViewMode #-}
putViewMode :: MonadState AppState m => ViewMode -> m ()
putViewMode p =
  getGame >>= \g -> putGame $ g {getViewMode' = p}

{-# INLINABLE putWorld #-}
putWorld :: MonadState AppState m => World -> m ()
putWorld w = getGameState >>= \g ->
  putGameState g {getPreviousWorld = w}

{-# INLINABLE takeKeys #-}
takeKeys :: MonadState AppState m => Int -> m [ParticleSystemKey]
takeKeys n
  | n <= 0 = return []
  | otherwise =
      get >>= \(AppState a c d e f key g) -> do
        let endKey = key + fromIntegral n
        put $ AppState a c d e f endKey g
        return [key..pred endKey]

{-# INLINABLE addParticleSystems #-}
addParticleSystems :: MonadState AppState m
                   => [Prioritized ParticleSystem]
                   -> m ()
addParticleSystems l = do
  keys <- takeKeys $ length l
  let ps2 = fromList $ zip keys l
  getWorld >>= \w -> putWorld $ w {getParticleSystems = union (getParticleSystems w) ps2}

{-# INLINABLE updateOneParticleSystem #-}
updateOneParticleSystem :: MonadState AppState m
                        => ParticleSystemKey
                        -> Time Point System
                        -> m ()
updateOneParticleSystem key t =
  getWorld >>= \w -> do
    let tps = systemTimePointToParticleSystemTimePoint t
        newSystems =
          updateWithKey
            (\_ (Prioritized p ps) -> fmap (Prioritized p) $ updateParticleSystem tps ps)
            key $ getParticleSystems w
    putWorld $ w {getParticleSystems = newSystems}

{-# INLINABLE putClientState #-}
putClientState :: MonadState AppState m => ClientState -> m ()
putClientState i =
  getGame >>= \g -> putGame $ g { getClientState' = i}

{-# INLINABLE stateChat #-}
stateChat :: MonadState AppState m => (Chat -> Chat) -> m ()
stateChat f =
  getGame >>= \g -> putGame $ g { getChat = f $ getChat g }

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
  mode <- getViewMode
  screen <- getCurScreen
  return $ EnvFunctions (environmentInteraction world mode screen scope) envDistance
