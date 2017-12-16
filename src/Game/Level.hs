{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game.Level
    ( Level(..)
    , LevelFinished(..)
    , renderLevelMessage
    , isLevelFinished
    , renderLevelState
    , MessageState(..)
    , GameStops(..)
    , messageDeadline
    , getEventForMaybeDeadline
    -- | reexports
    , module Game.Level.Types
    ) where

import           Imajuscule.Prelude

import           Data.Text( pack )
import           System.Timeout( timeout )

import           Game.Color
import           Game.Deadline( Deadline(..) )
import           Game.Event
import           Game.Level.Types
import           Game.World( BattleShip(..)
                      , Number(..)
                      , World(..) )

import           Geo( Direction(..) )
import           Geo.Discrete

import           IO.NonBlocking
import           IO.Blocking( getCharThenFlush )
import           IO.Types

import           Render.Draw
import           Timing( UTCTime
                       , KeyTime(..)
                       , diffTimeSecToMicros
                       , diffUTCTime
                       , addUTCTime )

import           Util( showListOrSingleton )

eventFromChar :: Level -> Either Key Char -> Event
eventFromChar (Level n _ finished) char =
  case finished of
    Nothing -> Game.Event.eventFromChar char
    Just (LevelFinished stop _ ContinueMessage) ->
      case stop of
        Won      -> if n < lastLevel then StartLevel (succ n) else EndGame
        (Lost _) -> StartLevel firstLevel
    _ -> Nonsense -- between level end and proposal to continue


isLevelFinished :: World e -> Int -> Int -> TimedEvent -> Maybe LevelFinished
isLevelFinished (World _ _ (BattleShip _ ammo safeTime collisions) _ _ _) sumNumbers target (TimedEvent lastEvent t) =
    maybe Nothing (\stop -> Just $ LevelFinished stop t InfoMessage) allChecks
  where
    allChecks = checkShipCollision <|> checkSum <|> checkAmmo

    checkShipCollision = case lastEvent of
      (Timeout GameStep _) ->
        maybe
          (case map (\(Number _ n) -> n) collisions of
            [] -> Nothing
            l  -> Just $ Lost $ "collision with " <> showListOrSingleton l)
          (const Nothing)
          safeTime
      _ -> Nothing -- this optimization is to not re-do the check when nothing has moved

    checkSum = case compare sumNumbers target of
      LT -> Nothing
      EQ -> Just Won
      GT -> Just $ Lost $ pack $ show sumNumbers ++ " is bigger than " ++ show target
    checkAmmo
      | ammo <= 0 = Just $ Lost $ pack "no ammo left"
      | otherwise = Nothing

messageDeadline :: Level -> UTCTime -> Maybe Deadline
messageDeadline (Level _ _ mayLevelFinished) t =
  maybe Nothing
  (\(LevelFinished _ timeFinished messageType) ->
    case messageType of
      InfoMessage ->
        let finishedSinceSeconds = diffUTCTime t timeFinished
            delay = 2
            nextMessageStep = addUTCTime (delay - finishedSinceSeconds) t
        in  Just $ Deadline (KeyTime nextMessageStep) MessageStep
      ContinueMessage -> Nothing)
  mayLevelFinished

getEventForMaybeDeadline :: Level -> Maybe Deadline -> UTCTime -> IO Event
getEventForMaybeDeadline level mayDeadline curTime =
  case mayDeadline of
    (Just (Deadline k@(KeyTime deadline) deadlineType)) -> do
      let
        timeToDeadlineMicros = diffTimeSecToMicros $ diffUTCTime deadline curTime
      eventWithinDurationMicros level timeToDeadlineMicros k deadlineType
    Nothing -> Game.Level.eventFromChar level <$> getCharThenFlush

eventWithinDurationMicros :: Level -> Int -> KeyTime -> Step -> IO Event
eventWithinDurationMicros level durationMicros k step =
  (\case
    Nothing   -> Timeout step k
    Just char -> Game.Level.eventFromChar level char
    ) <$> getCharWithinDurationMicros durationMicros step

getCharWithinDurationMicros :: Int -> Step -> IO (Maybe (Either Key Char))
getCharWithinDurationMicros durationMicros step =
  if durationMicros < 0
    -- overdue
    then
      if priority step < userEventPriority
        then
          tryGetCharThenFlush
        else
          return Nothing
    else
      timeout durationMicros getCharThenFlush

{-# INLINABLE renderLevelState #-}
renderLevelState :: (Draw e) => Coords -> Int -> LevelFinished -> ReaderT e IO ()
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
renderLevelMessage :: (Draw e) => Level -> Coords -> ReaderT e IO ()
renderLevelMessage (Level level _ levelState) rightMiddle =
  mapM_ (renderLevelState rightMiddle level) levelState
