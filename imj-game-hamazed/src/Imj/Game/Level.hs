{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Level
    ( renderLevelMessage
    , renderLevelState
    , messageDeadline
    , getEventForMaybeDeadline
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           System.Timeout( timeout )

import           Imj.Draw
import           Imj.Game.Color
import           Imj.Game.Event
import           Imj.Game.Level.Types
import           Imj.Geo.Discrete
import           Imj.Key.NonBlocking
import           Imj.Key.Blocking( getKeyThenFlush )
import           Imj.Key.Types
import           Imj.Timing

eventFromKey' :: Level -> Key -> Maybe Event
eventFromKey' (Level n _ finished) key =
  case finished of
    Nothing -> eventFromKey key
    Just (LevelFinished stop _ ContinueMessage) -> Just $
      case stop of
        Won      -> if n < lastLevel then StartLevel (succ n) else EndGame
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
            nextMessageStep = addSystemTime (delay - finishedSinceSeconds) t
        in  Just $ Deadline (KeyTime nextMessageStep) DisplayContinueMessage
      ContinueMessage -> Nothing)
  mayLevelFinished

-- | Returns a /player event/ or the 'Event' associated to the 'Deadline' if the
-- 'Deadline' expired before the /player/ could press a 'Key'.
getEventForMaybeDeadline :: Level
                         -- ^ Current level
                         -> Maybe Deadline
                         -- ^ May contain a 'Deadline'
                         -> SystemTime
                         -- ^ Current time
                         -> IO (Maybe Event)
getEventForMaybeDeadline level mayDeadline curTime =
  case mayDeadline of
    (Just (Deadline k@(KeyTime deadline) deadlineType)) -> do
      let
        timeToDeadlineMicros = diffTimeSecToMicros $ diffSystemTime deadline curTime
      eventWithinDurationMicros level timeToDeadlineMicros k deadlineType
    Nothing -> eventFromKey' level <$> getKeyThenFlush

eventWithinDurationMicros :: Level -> Int -> KeyTime -> DeadlineType -> IO (Maybe Event)
eventWithinDurationMicros level durationMicros k step =
  (\case
    Just key -> eventFromKey' level key
    _ -> Just $ Timeout (Deadline k step)
    ) <$> getCharWithinDurationMicros durationMicros step

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

{-# INLINABLE renderLevelState #-}
renderLevelState :: (Draw e, MonadReader e m, MonadIO m)
                 => Coords
                 -> Int
                 -> LevelFinished
                 -> m ()
renderLevelState s level (LevelFinished stop _ messageState) = do
  let topLeft = translateInDir RIGHT s
      stopMsg = case stop of
        (Lost reason) -> "You Lose (" <> reason <> ")"
        Won           -> "You Win!"
  drawTxt stopMsg topLeft (messageColor stop)
  when (messageState == ContinueMessage) $
    drawTxt
      (if level == lastLevel
        then
          "You reached the end of the game!"
        else
          let action = case stop of
                            (Lost _) -> "restart"
                            Won      -> "continue"
          in "Hit a key to " <> action <> " ...")
      (move 2 Down topLeft) neutralMessageColor


{-# INLINABLE renderLevelMessage #-}
renderLevelMessage :: (Draw e, MonadReader e m, MonadIO m)
                   => Level
                   -> Coords
                   -> m ()
renderLevelMessage (Level level _ levelState) rightMiddle =
  mapM_ (renderLevelState rightMiddle level) levelState
