{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Level
    ( drawLevelMessage
    , drawLevelState
    , messageDeadline
    , getEventForDeadline
    , eventFromKey'
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.State
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.KeysMaps
import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Render
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.UI.Colored
import           Imj.Input.Types
import           Imj.Input.FromMonadReader
import           Imj.Timing

eventFromKey' :: (MonadState AppState m)
              => Key -> m (Maybe Event)
eventFromKey' key =
  getGameState >>= \(GameState _ _ _ _ (Level n _ finished) _ _) ->
    return $ case finished of
      Nothing -> eventFromKey key
      Just (LevelFinished stop _ ContinueMessage) -> Just $
        case stop of
          Won -> if n < lastLevel
                   then
                     StartLevel (succ n)
                   else
                     EndGame
          (Lost _) -> StartLevel firstLevel
      _ -> Nothing -- between level end and proposal to continue

messageDeadline :: Level -> Maybe Deadline
messageDeadline (Level _ _ mayLevelFinished) =
  maybe Nothing
  (\(LevelFinished _ timeFinished messageType) ->
    case messageType of
      InfoMessage ->
        let delay = TimeSpec 2 0
            nextMessageStep = delay + timeFinished
        in  Just $ Deadline (KeyTime nextMessageStep) continueMsgPriority DisplayContinueMessage
      ContinueMessage -> Nothing)
  mayLevelFinished

-- | Returns a /player event/ or the 'Event' associated to the 'Deadline' if the
-- 'Deadline' expired before the /player/ could press a 'Key'.
{-# INLINABLE getEventForDeadline #-}
getEventForDeadline :: (MonadState AppState m, PlayerInput i, MonadReader i m, MonadIO m)
                    => Deadline
                    -> m (Maybe Event)
getEventForDeadline d@(Deadline (KeyTime deadlineTime) _ _) = do
  curTime <- liftIO getSystemTime
  let
    timeToDeadlineMicros = diffTimeSecToMicros deadlineTime curTime
  eventWithinDurationMicros curTime timeToDeadlineMicros d

{-# INLINABLE eventWithinDurationMicros #-}
eventWithinDurationMicros :: (MonadState AppState m, PlayerInput i, MonadReader i m, MonadIO m)
                          => TimeSpec -> Int64 -> Deadline -> m (Maybe Event)
eventWithinDurationMicros curTime durationMicros d =
  getCharWithinDurationMicros curTime durationMicros d >>= \case
    Just key -> eventFromKey' key
    _ -> return $ Just $ Timeout d

{-# INLINABLE getCharWithinDurationMicros #-}
getCharWithinDurationMicros :: (PlayerInput i, MonadReader i m, MonadIO m)
                            => TimeSpec -> Int64 -> Deadline -> m (Maybe Key)
getCharWithinDurationMicros curTime durationMicros (Deadline _ deadlinePriority _) =
  if durationMicros < 0
    -- overdue
    then
      if playerPriority > deadlinePriority
        then
          tryGetPlayerKey
        else
          return Nothing
    else
      getPlayerKeyTimeout curTime durationMicros

{-# INLINABLE drawLevelState #-}
drawLevelState :: (Draw e, MonadReader e m, MonadIO m)
               => Coords Pos
               -> Int
               -> LevelFinished
               -> m ()
drawLevelState centerRef level (LevelFinished stop _ messageState) = do
  let stopMsg = case stop of
        (Lost reason) -> "You Lose (" <> reason <> ")"
        Won           -> "You Win!"
  drawAligned_ (Colored (messageColor stop) stopMsg) (mkCentered centerRef)
  when (messageState == ContinueMessage) $
    drawAligned_ (Colored neutralMessageColor $
      if level == lastLevel
        then
          "You reached the end of the game!"
        else
          let action = case stop of
                            (Lost _) -> "restart"
                            Won      -> "continue"
          in "Press a key to " <> action <> " ..." :: Text)
           (mkCentered $ move 2 Down centerRef)


{-# INLINABLE drawLevelMessage #-}
drawLevelMessage :: (Draw e, MonadReader e m, MonadIO m)
                 => Level
                 -> Coords Pos
                 -> m ()
drawLevelMessage (Level level _ levelState) ref =
  mapM_ (drawLevelState ref level) levelState
