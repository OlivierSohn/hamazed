{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Modify
      ( -- * Access
        getGameState
      , getIGame
      , getPlayers
      , getPlayer
      , getChatMode
      , getMyId
      , getChat
      , getServerContent
      , getCurScreen
      , getLastRenderTime
      , hasVisibleNonRenderedUpdates
      -- * Modify
      , addParticleSystems
      , putGame
      , putAnimation
      , putCurScreen
      , putGameState
      , putIGame
      , putPlayer
      , putPlayers
      , putGameConnection
      , putServerContent
      , putDrawnState
      , stateChat
      , keyIsPressed
      , takeKeyPressed
      , addKeyPressed
      ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State.Strict(gets, modify', state)
import           Control.Monad.Reader.Class(MonadReader)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Data.Map.Strict((!?),Map)
import qualified Graphics.UI.GLFW as GLFW(Key(..))

import           Imj.Event
import           Imj.Game.Class
import           Imj.Graphics.Screen
import           Imj.Input.Types
import           Imj.Server.Class
import           Imj.ServerView.Types

import           Imj.Game.Priorities
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.Text.ColorString hiding(putStrLn, putStr)
import           Imj.Graphics.UI.Animation.Types
import           Imj.Graphics.UI.Chat


{-# INLINABLE getGameState #-}
getGameState :: MonadState (AppState g) m => m (GameState g)
getGameState = getGameState' <$> gets game

{-# INLINABLE getIGame #-}
getIGame :: MonadState (AppState g) m => m (Maybe g)
getIGame = _game <$> getGameState

{-# INLINABLE getServerView #-}
getServerView :: MonadState (AppState g) m => m (ServerView (ValuesT (ServerT g)))
getServerView = getServerView' <$> gets game

{-# INLINABLE getChatMode #-}
getChatMode :: MonadState (AppState g) m => m IsEditing
getChatMode = getIsEditing <$> getChat

{-# INLINABLE getChat #-}
getChat :: MonadState (AppState g) m => m Chat
getChat = getChat' <$> gets game

{-# INLINABLE getCurScreen #-}
getCurScreen :: MonadState (AppState g) m => m Screen
getCurScreen = getScreen <$> gets game

{-# INLINABLE putCurScreen #-}
putCurScreen :: MonadState (AppState g) m => Screen -> m ()
putCurScreen s = gets game >>= \g -> putGame $ g { getScreen = s }

{-# INLINABLE getLastRenderTime #-}
getLastRenderTime :: MonadState (AppState g) m => m (Time Point System)
getLastRenderTime = gets timeAfterRender

{-# INLINABLE putGame #-}
putGame :: MonadState (AppState g) m => Game g -> m ()
putGame g = modify' $ \s -> s { game = g }

{-# INLINABLE putAnimation #-}
putAnimation :: (GameLogicT e ~ g
               , MonadState (AppState (GameLogicT e)) m
               , MonadReader e m, Client e
               , MonadIO m)
             => UIAnimation
             -> m ()
putAnimation a = do
  getGameState >>= \g -> putGameState $ g {_anim = a}
  maybe onAnimFinished (const $ return ()) $ _deadline $ getProgress a

{-# INLINABLE putIGame #-}
putIGame :: MonadState (AppState s) m => s -> m ()
putIGame a =
  getGameState >>= \g -> putGameState $ g {_game = Just a}

{-# INLINABLE putServer #-}
putServer :: MonadState (AppState g) m => (ServerView (ValuesT (ServerT g))) -> m ()
putServer s =
  gets game >>= \g -> putGame $ g {getServerView' = s}

{-# INLINABLE putGameState #-}
putGameState :: MonadState (AppState g) m => GameState g -> m ()
putGameState s =
  gets game >>= \g -> putGame $ g {getGameState' = s}

{-# INLINABLE putGameConnection #-}
putGameConnection :: MonadState (AppState g) m => Either Text ClientId -> m ()
putGameConnection c =
  gets game >>= \g -> putGame $ g {connection' = Just c}

{-# INLINABLE getMyId #-}
getMyId :: MonadState (AppState g) m => m (Maybe ClientId)
getMyId = maybe Nothing (either (const Nothing) Just) . connection' <$> gets game

{-# INLINABLE putServerContent #-}
putServerContent :: MonadState (AppState g) m => ValuesT (ServerT g) -> m ()
putServerContent p =
  getServerView >>= \s@(ServerView _ c) ->
    putServer s { serverContent = c { cachedValues = Just p } }

{-# INLINABLE getServerContent #-}
getServerContent :: MonadState (AppState g) m => m (Maybe (ValuesT (ServerT g)))
getServerContent =
  cachedValues . serverContent <$> getServerView

{-# INLINABLE getPlayers #-}
getPlayers :: MonadState (AppState g) m => m (Map ClientId (Player g))
getPlayers = getPlayers' <$> gets game

{-# INLINABLE getPlayer #-}
getPlayer :: MonadState (AppState g) m => ClientId -> m (Maybe (Player g))
getPlayer i = flip (!?) i <$> getPlayers

{-# INLINABLE putPlayers #-}
putPlayers :: MonadState (AppState g) m => Map ClientId (Player g) -> m ()
putPlayers m = gets game >>= \g -> putGame g {getPlayers' = m}

{-# INLINABLE putPlayer #-}
putPlayer :: MonadState (AppState g) m => ClientId -> Player g -> m ()
putPlayer sid player = getPlayers >>= \names -> putPlayers $ Map.insert sid player names

{-# INLINABLE takeKeys #-}
takeKeys :: MonadState (AppState g) m => Int -> m [ParticleSystemKey]
takeKeys n
  | n <= 0 = return []
  | otherwise =
      state $ \s ->
        let key = nextParticleSystemKey s
            endKey = key + fromIntegral n
        in ([key..pred endKey], s {nextParticleSystemKey = endKey })

{-# INLINABLE addParticleSystems #-}
addParticleSystems :: MonadState (AppState s) m
                   => [Prioritized ParticleSystem]
                   -> m ()
addParticleSystems l = do
  keys <- takeKeys $ length l
  let ps2 = Map.fromDistinctAscList $ zip keys l
  gets game >>= \g -> putGame $ g {gameParticleSystems = Map.union (gameParticleSystems g) ps2}

{-# INLINABLE putDrawnState #-}
putDrawnState :: (MonadState (AppState g) m)
              => [(ColorString, AnimatedLine)]
              -> m ()
putDrawnState i =
  gets game >>= \g -> putGame $ g { getDrawnClientState = i }

{-# INLINABLE stateChat #-}
stateChat :: MonadState (AppState g) m => (Chat -> (Chat, a)) -> m a
stateChat f =
  gets game >>= \g -> do
    let (newChat, v) = f $ getChat' g
    putGame $ g { getChat' = newChat }
    return v

{-# INLINABLE hasVisibleNonRenderedUpdates #-}
hasVisibleNonRenderedUpdates :: MonadState (AppState g) m => m Bool
hasVisibleNonRenderedUpdates =
  visible <$> gets eventsGroup

-- | Returns 'True' if a keypress event was handled by 'mapStateKey' without matching release.
{-# INLINABLE keyIsPressed #-}
keyIsPressed :: MonadState (AppState g) m => GLFW.Key -> m Bool
keyIsPressed k = Set.member k <$> gets pressedKeys

-- | Same as 'keyIsPressed', and marks the key as having a matching release,
-- i.e next call to 'keyIsPressed' will return 'False'.
{-# INLINABLE takeKeyPressed #-}
takeKeyPressed :: MonadState (AppState g) m
               => GLFW.Key -> m Bool
takeKeyPressed k = state $ \s ->
  let prev = pressedKeys s
      szPrev = Set.size prev
      cur = Set.delete k prev
      szCur = Set.size cur
  in (szCur < szPrev, s { pressedKeys = cur })

-- After this call, 'keyIsPressed' @k@  will return 'True'
{-# INLINABLE addKeyPressed #-}
addKeyPressed :: MonadState (AppState g) m
              => GLFW.Key -> m ()
addKeyPressed k = modify' $ \s ->
  s { pressedKeys = Set.insert k $ pressedKeys s }
