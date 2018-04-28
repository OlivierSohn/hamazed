{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Hamazed.State.Types
      (
      -- * AppState type
        AppState(..)
      , RecordMode(..)
      , OccurencesHist(..)
      , Occurences(..)
      , EventRepr(..)
      -- * Access
      , getGameState
      , getPlayers
      , getPlayer
      , getLevel
      , getLevelOutcome
      , getChatMode
      , getMyShipId
      , getGameConnection
      , getChat
      , getViewMode
      , getWorld
      , getWorldParameters
      , getCurScreen
      , getLastRenderTime
      , hasVisibleNonRenderedUpdates
      , envFunctions
      -- * Modify
      , putGame
      , putGameState
      , putPlayer
      , putPlayers
      , putLevel
      , putLevelOutcome
      , putViewMode
      , putGameConnection
      , putWorld
      , putWorldParameters
      , putAnimation
      , putDrawnState
      , stateChat
      , addParticleSystems
      , updateOneParticleSystem
      -- * reexports
      , MonadState
      , gets
      ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State.Strict(gets, state, modify')
import           Data.Map.Strict(fromDistinctAscList, union, updateWithKey, insert, (!?))

import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.UI.Animation
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Server.Types

import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.ParticleSystem.Design.Update
import           Imj.Graphics.Text.ColorString

data Occurences a = Occurences {
    _occurencesCount :: {-# UNPACK #-} !Int
  , _occurencesItem :: {-unpack sum-} !EventRepr
} deriving(Generic, Show)

data AppState  = AppState {
    timeAfterRender :: !(Time Point System)
  , game :: !Game
  , eventsGroup :: !EventGroup
  , _appStateEventHistory :: !OccurencesHist
  -- ^ Can record which events where handled.
  , _appStateRecordEvents :: !RecordMode
  -- ^ Should the handled events be recorded?
  , nextParticleSystemKey :: !ParticleSystemKey
  , _appStateDebug :: {-unpack sum-} !Bool
  -- ^ Print times and group information in the terminal.
}

data RecordMode = Record
                | DontRecord
                deriving(Eq)

data OccurencesHist = OccurencesHist {
    _occurencesHistList :: ![Occurences EventRepr]
  , _occurencesHistTailStr :: !ColorString
} deriving(Generic, Show)

data EventRepr = Laser'
               | PeriodicMotion'
               | MoveFlyingItems'
               | AnimateParticleSystem'
               | AnimateUI'
               | WorldRequest'
               | ChangeLevel'
               | Command'
               | CycleRenderingOptions'
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


{-# INLINABLE getGameState #-}
getGameState :: MonadState AppState m => m GameState
getGameState = getGameState' <$> gets game

{-# INLINABLE getServer #-}
getServer :: MonadState AppState m => m Server
getServer = getServer' <$> gets game

{-# INLINABLE getViewMode #-}
getViewMode :: MonadState AppState m => m ViewMode
getViewMode = getViewMode' <$> getGameState

{-# INLINABLE getChatMode #-}
getChatMode :: MonadState AppState m => m IsEditing
getChatMode = getIsEditing <$> getChat

{-# INLINABLE getChat #-}
getChat :: MonadState AppState m => m Chat
getChat = getChat' <$> gets game

{-# INLINABLE getWorld #-}
getWorld :: MonadState AppState m => m World
getWorld = currentWorld <$> getGameState

{-# INLINABLE getCurScreen #-}
getCurScreen :: MonadState AppState m => m Screen
getCurScreen = getScreen <$> getGameState

{-# INLINABLE getLevel #-}
getLevel :: MonadState AppState m => m Level
getLevel = getGameLevel <$> getGameState

{-# INLINABLE putLevel #-}
putLevel :: MonadState AppState m => Level -> m ()
putLevel l = getGameState >>= \s -> putGameState s { getGameLevel = l }

{-# INLINABLE getLevelOutcome #-}
getLevelOutcome :: MonadState AppState m => m (Maybe LevelOutcome)
getLevelOutcome = getLevelOutcome' <$> getLevel

{-# INLINABLE putLevelOutcome #-}
putLevelOutcome :: MonadState AppState m => Maybe LevelOutcome -> m ()
putLevelOutcome l = getLevel >>= \s -> putLevel s { getLevelOutcome' = l }

{-# INLINABLE getLastRenderTime #-}
getLastRenderTime :: MonadState AppState m => m (Time Point System)
getLastRenderTime = gets timeAfterRender

{-# INLINABLE putGame #-}
putGame :: MonadState AppState m => Game -> m ()
putGame g = modify' $ \s -> s { game = g }

{-# INLINABLE putServer #-}
putServer :: MonadState AppState m => Server -> m ()
putServer s =
  gets game >>= \g -> putGame $ g {getServer' = s}

{-# INLINABLE putGameState #-}
putGameState :: MonadState AppState m => GameState -> m ()
putGameState s =
  gets game >>= \g -> putGame $ g {getGameState' = s}

{-# INLINABLE putAnimation #-}
putAnimation :: MonadState AppState m => UIAnimation -> m ()
putAnimation a =
  getGameState >>= \s -> putGameState $ s {getUIAnimation = a}

{-# INLINABLE putGameConnection #-}
putGameConnection :: MonadState AppState m => ConnectionStatus -> m ()
putGameConnection c =
  gets game >>= \g -> putGame $ g {connection' = c}

{-# INLINABLE getGameConnection #-}
getGameConnection :: MonadState AppState m => m ConnectionStatus
getGameConnection = connection' <$> gets game

{-# INLINABLE getMyShipId #-}
getMyShipId :: MonadState AppState m => m (Maybe ShipId)
getMyShipId =
  (\case
    Connected myId -> Just myId
    _ -> Nothing) <$> getGameConnection

{-# INLINABLE putViewMode #-}
putViewMode :: MonadState AppState m => ViewMode -> m ()
putViewMode p =
  getGameState >>= \g -> putGameState $ g {getViewMode' = p}

{-# INLINABLE putWorld #-}
putWorld :: MonadState AppState m => World -> m ()
putWorld w = getGameState >>= \g -> putGameState g {currentWorld = w}

{-# INLINABLE putWorldParameters #-}
putWorldParameters :: MonadState AppState m => WorldParameters -> m ()
putWorldParameters p =
  getServer >>= \s@(Server _ c) ->
    putServer s { serverContent = c { serverWorldParameters = Just p } }

{-# INLINABLE getWorldParameters #-}
getWorldParameters :: MonadState AppState m => m (Maybe WorldParameters)
getWorldParameters =
  serverWorldParameters . serverContent <$> getServer

{-# INLINABLE getPlayers #-}
getPlayers :: MonadState AppState m => m (Map ShipId Player)
getPlayers = getPlayers' <$> getGameState

{-# INLINABLE getPlayer #-}
getPlayer :: MonadState AppState m => ShipId -> m (Maybe Player)
getPlayer i = flip (!?) i <$> getPlayers

{-# INLINABLE putPlayers #-}
putPlayers :: MonadState AppState m => Map ShipId Player -> m ()
putPlayers m = getGameState >>= \g -> putGameState g {getPlayers' = m}

{-# INLINABLE putPlayer #-}
putPlayer :: MonadState AppState m => ShipId -> Player -> m ()
putPlayer sid player = getPlayers >>= \names -> putPlayers $ insert sid player names

{-# INLINABLE takeKeys #-}
takeKeys :: MonadState AppState m => Int -> m [ParticleSystemKey]
takeKeys n
  | n <= 0 = return []
  | otherwise =
      state $ \s ->
        let key = nextParticleSystemKey s
            endKey = key + fromIntegral n
        in ([key..pred endKey], s {nextParticleSystemKey = endKey })

{-# INLINABLE addParticleSystems #-}
addParticleSystems :: MonadState AppState m
                   => [Prioritized ParticleSystem]
                   -> m ()
addParticleSystems l = do
  keys <- takeKeys $ length l
  let ps2 = fromDistinctAscList $ zip keys l
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

{-# INLINABLE putDrawnState #-}
putDrawnState :: (MonadState AppState m)
              => [(ColorString, AnimatedLine)]
              -> m ()
putDrawnState i =
  getGameState >>= \gs -> putGameState $ gs { getDrawnClientState = i }

{-# INLINABLE stateChat #-}
stateChat :: MonadState AppState m => (Chat -> (Chat, a)) -> m a
stateChat f =
  gets game >>= \g -> do
    let (newChat, v) = f $ getChat' g
    putGame $ g { getChat' = newChat }
    return v

{-# INLINABLE hasVisibleNonRenderedUpdates #-}
hasVisibleNonRenderedUpdates :: MonadState AppState m => m Bool
hasVisibleNonRenderedUpdates =
  visible <$> gets eventsGroup

-- | Creates environment functions taking into account a 'World' and 'Scope'
{-# INLINABLE envFunctions #-}
envFunctions :: (MonadState AppState m)
             => Scope -> m EnvFunctions
envFunctions scope = do
  world <- getWorld
  mode <- getViewMode
  screen <- getCurScreen
  return $ EnvFunctions (environmentInteraction world mode screen scope) envDistance
