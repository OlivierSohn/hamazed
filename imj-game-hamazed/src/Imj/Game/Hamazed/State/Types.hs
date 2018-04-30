{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Hamazed.State.Types
      (
      -- * (AppState evt) type
        AppState(..)
      , RecordMode(..)
      , OccurencesHist(..)
      , Occurences(..)
      , EventCategory(..)
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
      , putCurScreen
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
import           Imj.Graphics.Screen
import           Imj.Graphics.UI.Animation
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.ServerView.Types

import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.ParticleSystem.Design.Update
import           Imj.Graphics.Text.ColorString

data Occurences a = Occurences {
    _occurencesCount :: {-# UNPACK #-} !Int
  , _occurencesItem :: {-unpack sum-} !EventCategory
} deriving(Generic, Show)

data AppState evt = AppState {
    timeAfterRender :: !(Time Point System)
  , game :: !Game
  , eventsGroup :: !(EventGroup Hamazed (Event evt)) -- TODO Hamazed should define the type of event.
  , _appStateEventHistory :: !OccurencesHist
  -- ^ Can record which events where handled, for debugging purposes.
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
    _occurencesHistList :: ![Occurences EventCategory]
  , _occurencesHistTailStr :: !ColorString
} deriving(Generic, Show)


{-# INLINABLE getGameState #-}
getGameState :: MonadState (AppState evt) m => m GameState
getGameState = getGameState' <$> gets game

{-# INLINABLE getServerView #-}
getServerView :: MonadState (AppState evt) m => m (ServerView ColorScheme WorldParameters)
getServerView = getServerView' <$> gets game

{-# INLINABLE getViewMode #-}
getViewMode :: MonadState (AppState evt) m => m ViewMode
getViewMode = getViewMode' <$> getGameState

{-# INLINABLE getChatMode #-}
getChatMode :: MonadState (AppState evt) m => m IsEditing
getChatMode = getIsEditing <$> getChat

{-# INLINABLE getChat #-}
getChat :: MonadState (AppState evt) m => m Chat
getChat = getChat' <$> gets game

{-# INLINABLE getWorld #-}
getWorld :: MonadState (AppState evt) m => m World
getWorld = currentWorld <$> getGameState

{-# INLINABLE getCurScreen #-}
getCurScreen :: MonadState (AppState evt) m => m Screen
getCurScreen = getScreen <$> gets game

{-# INLINABLE putCurScreen #-}
putCurScreen :: MonadState (AppState evt) m => Screen -> m ()
putCurScreen s = gets game >>= \g -> putGame $ g { getScreen = s }

{-# INLINABLE getLevel #-}
getLevel :: MonadState (AppState evt) m => m Level
getLevel = getGameLevel <$> getGameState

{-# INLINABLE putLevel #-}
putLevel :: MonadState (AppState evt) m => Level -> m ()
putLevel l = getGameState >>= \s -> putGameState s { getGameLevel = l }

{-# INLINABLE getLevelOutcome #-}
getLevelOutcome :: MonadState (AppState evt) m => m (Maybe LevelOutcome)
getLevelOutcome = getLevelOutcome' <$> getLevel

{-# INLINABLE putLevelOutcome #-}
putLevelOutcome :: MonadState (AppState evt) m => Maybe LevelOutcome -> m ()
putLevelOutcome l = getLevel >>= \s -> putLevel s { getLevelOutcome' = l }

{-# INLINABLE getLastRenderTime #-}
getLastRenderTime :: MonadState (AppState evt) m => m (Time Point System)
getLastRenderTime = gets timeAfterRender

{-# INLINABLE putGame #-}
putGame :: MonadState (AppState evt) m => Game -> m ()
putGame g = modify' $ \s -> s { game = g }

{-# INLINABLE putServer #-}
putServer :: MonadState (AppState evt) m => (ServerView ColorScheme WorldParameters) -> m ()
putServer s =
  gets game >>= \g -> putGame $ g {getServerView' = s}

{-# INLINABLE putGameState #-}
putGameState :: MonadState (AppState evt) m => GameState -> m ()
putGameState s =
  gets game >>= \g -> putGame $ g {getGameState' = s}

{-# INLINABLE putAnimation #-}
putAnimation :: MonadState (AppState evt) m => UIAnimation -> m ()
putAnimation a =
  getGameState >>= \s -> putGameState $ s {getUIAnimation = a}

{-# INLINABLE putGameConnection #-}
putGameConnection :: MonadState (AppState evt) m => ConnectionStatus -> m ()
putGameConnection c =
  gets game >>= \g -> putGame $ g {connection' = c}

{-# INLINABLE getGameConnection #-}
getGameConnection :: MonadState (AppState evt) m => m ConnectionStatus
getGameConnection = connection' <$> gets game

{-# INLINABLE getMyShipId #-}
getMyShipId :: MonadState (AppState evt) m => m (Maybe ShipId)
getMyShipId =
  (\case
    Connected myId -> Just myId
    _ -> Nothing) <$> getGameConnection

{-# INLINABLE putViewMode #-}
putViewMode :: MonadState (AppState evt) m => ViewMode -> m ()
putViewMode p =
  getGameState >>= \g -> putGameState $ g {getViewMode' = p}

{-# INLINABLE putWorld #-}
putWorld :: MonadState (AppState evt) m => World -> m ()
putWorld w = getGameState >>= \g -> putGameState g {currentWorld = w}

{-# INLINABLE putWorldParameters #-}
putWorldParameters :: MonadState (AppState evt) m => WorldParameters -> m ()
putWorldParameters p =
  getServerView >>= \s@(ServerView _ c) ->
    putServer s { serverContent = c { cachedContent = Just p } }

{-# INLINABLE getWorldParameters #-}
getWorldParameters :: MonadState (AppState evt) m => m (Maybe WorldParameters)
getWorldParameters =
  cachedContent . serverContent <$> getServerView

{-# INLINABLE getPlayers #-}
getPlayers :: MonadState (AppState evt) m => m (Map ShipId Player)
getPlayers = getPlayers' <$> gets game

{-# INLINABLE getPlayer #-}
getPlayer :: MonadState (AppState evt) m => ShipId -> m (Maybe Player)
getPlayer i = flip (!?) i <$> getPlayers

{-# INLINABLE putPlayers #-}
putPlayers :: MonadState (AppState evt) m => Map ShipId Player -> m ()
putPlayers m = gets game >>= \g -> putGame g {getPlayers' = m}

{-# INLINABLE putPlayer #-}
putPlayer :: MonadState (AppState evt) m => ShipId -> Player -> m ()
putPlayer sid player = getPlayers >>= \names -> putPlayers $ insert sid player names

{-# INLINABLE takeKeys #-}
takeKeys :: MonadState (AppState evt) m => Int -> m [ParticleSystemKey]
takeKeys n
  | n <= 0 = return []
  | otherwise =
      state $ \s ->
        let key = nextParticleSystemKey s
            endKey = key + fromIntegral n
        in ([key..pred endKey], s {nextParticleSystemKey = endKey })

{-# INLINABLE addParticleSystems #-}
addParticleSystems :: MonadState (AppState evt) m
                   => [Prioritized ParticleSystem]
                   -> m ()
addParticleSystems l = do
  keys <- takeKeys $ length l
  let ps2 = fromDistinctAscList $ zip keys l
  getWorld >>= \w -> putWorld $ w {getParticleSystems = union (getParticleSystems w) ps2}

{-# INLINABLE updateOneParticleSystem #-}
updateOneParticleSystem :: MonadState (AppState evt) m
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
putDrawnState :: (MonadState (AppState evt) m)
              => [(ColorString, AnimatedLine)]
              -> m ()
putDrawnState i =
  gets game >>= \g -> putGame $ g { getDrawnClientState = i }

{-# INLINABLE stateChat #-}
stateChat :: MonadState (AppState evt) m => (Chat -> (Chat, a)) -> m a
stateChat f =
  gets game >>= \g -> do
    let (newChat, v) = f $ getChat' g
    putGame $ g { getChat' = newChat }
    return v

{-# INLINABLE hasVisibleNonRenderedUpdates #-}
hasVisibleNonRenderedUpdates :: MonadState (AppState evt) m => m Bool
hasVisibleNonRenderedUpdates =
  visible <$> gets eventsGroup

-- | Creates environment functions taking into account a 'World' and 'Scope'
{-# INLINABLE envFunctions #-}
envFunctions :: (MonadState (AppState evt) m)
             => Scope -> m EnvFunctions
envFunctions scope = do
  world <- getWorld
  mode <- getViewMode
  screen <- getCurScreen
  return $ EnvFunctions (environmentInteraction world mode screen scope) envDistance
