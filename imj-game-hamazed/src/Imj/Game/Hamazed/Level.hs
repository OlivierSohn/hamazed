{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Level
    ( drawLevelMessage
    , drawLevelState
    , messageDeadline
    , getEventForMaybeDeadline
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           System.Timeout( timeout )

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
import           Imj.Input.NonBlocking
import           Imj.Input.Blocking
import           Imj.Input.Types
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

messageDeadline :: Level -> SystemTime -> Maybe Deadline
messageDeadline (Level _ _ mayLevelFinished) t =
  maybe Nothing
  (\(LevelFinished _ timeFinished messageType) ->
    case messageType of
      InfoMessage ->
        let finishedSinceSeconds = diffSystemTime t timeFinished
            delay = 2
            nextMessageStep = addToSystemTime (delay - finishedSinceSeconds) t
        in  Just $ Deadline (KeyTime nextMessageStep) DisplayContinueMessage
      ContinueMessage -> Nothing)
  mayLevelFinished

-- | Returns a /player event/ or the 'Event' associated to the 'Deadline' if the
-- 'Deadline' expired before the /player/ could press a 'Key'.
getEventForMaybeDeadline :: (MonadState AppState m, MonadIO m)
                         => Maybe Deadline
                         -- ^ May contain a 'Deadline'
                         -> SystemTime
                         -- ^ Current time
                         -> m (Maybe Event)
getEventForMaybeDeadline mayDeadline curTime =
  case mayDeadline of
    (Just (Deadline k@(KeyTime deadline) deadlineType)) -> do
      let
        timeToDeadlineMicros = diffTimeSecToMicros $ diffSystemTime deadline curTime
      eventWithinDurationMicros timeToDeadlineMicros k deadlineType
    Nothing -> liftIO getKeyThenFlush >>= eventFromKey'

eventWithinDurationMicros :: (MonadState AppState m, MonadIO m)
                          => Int -> KeyTime -> DeadlineType -> m (Maybe Event)
eventWithinDurationMicros durationMicros k step =
  liftIO (getCharWithinDurationMicros durationMicros step) >>= \case
    Just key -> eventFromKey' key
    _ -> return $ Just $ Timeout (Deadline k step)

getCharWithinDurationMicros :: Int -> DeadlineType -> IO (Maybe Key)
getCharWithinDurationMicros durationMicros step =
  if durationMicros < 0
    -- overdue
    then
      if playerEventPriority > deadlinePriority step
        then
          tryGetKeyThenFlush
        else
          return Nothing
    else
      timeout durationMicros getKeyThenFlush

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
