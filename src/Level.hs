{-# LANGUAGE LambdaCase #-}

module Level
    ( Level(..)
    , LevelFinished(..)
    , isLevelFinished
    , MessageState(..)
    , GameStops(..)
    , firstLevel
    , lastLevel
    , messageDeadline
    , getEventForMaybeDeadline
    ) where

import           Imajuscule.Prelude

import           System.IO( getChar )
import           System.Timeout( timeout )

import           Deadline( Deadline(..) )
import           Event( Event(..)
                      , eventFromChar
                      , Step(..)
                      , TimedEvent(..) )
import           Timing( UTCTime
                       , KeyTime(..)
                       , diffTimeSecToMicros
                       , diffUTCTime
                       , addUTCTime )
import           Util( showListOrSingleton )
import           World( BattleShip(..)
                      , Number(..)
                      , World(..) )

data Level = Level {
    _levelNumber :: !Int
  , _levelStatus :: !(Maybe LevelFinished)
}

lastLevel :: Int
lastLevel = 12

firstLevel :: Int
firstLevel = 1

data LevelFinished = LevelFinished {
    _levelFinishedResult :: !GameStops
  , _levelFinishedWhen :: !UTCTime
  , _levelFinishedCurrentMessage :: !MessageState
}

data MessageState = InfoMessage
                  | ContinueMessage
                  deriving(Eq, Show)

data GameStops = Lost String
               | Won


eventFromChar :: Level -> Char -> Event
eventFromChar (Level n finished) char = case finished of
  Nothing -> Event.eventFromChar char
  Just (LevelFinished stop _ ContinueMessage) ->
    case stop of
      Won      -> if n < lastLevel then StartLevel (succ n) else EndGame
      (Lost _) -> StartLevel firstLevel
  _ -> Nonsense -- between level end and proposal to continue


isLevelFinished :: World -> Int -> Int -> TimedEvent -> Maybe LevelFinished
isLevelFinished (World _ _ (BattleShip _ ammo safeTime collisions) _ _) sumNumbers target (TimedEvent lastEvent t) =
    maybe Nothing (\stop -> Just $ LevelFinished stop t InfoMessage) allChecks
  where
    allChecks = checkShipCollision <|> checkSum <|> checkAmmo

    checkShipCollision = case lastEvent of
      (Timeout GameStep _) ->
        maybe
          (case map (\(Number _ n) -> n) collisions of
            [] -> Nothing
            l  -> Just $ Lost $ "collision with " ++ showListOrSingleton l)
          (const Nothing)
          safeTime
      _ -> Nothing -- this optimization is to not re-do the check when nothing has moved

    checkSum = case compare sumNumbers target of
      LT -> Nothing
      EQ -> Just Won
      GT -> Just $ Lost $ "sum " ++ show sumNumbers ++ " is bigger than target " ++ show target
    checkAmmo
      | ammo <= 0 = Just $ Lost "no ammo left"
      | otherwise = Nothing

messageDeadline :: Level -> UTCTime -> Maybe Deadline
messageDeadline (Level _ mayLevelFinished) t =
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
    Nothing -> Level.eventFromChar level <$> getChar

eventWithinDurationMicros :: Level -> Int -> KeyTime -> Step -> IO Event
eventWithinDurationMicros level durationMicros k step =
  (\case
    Nothing   -> Timeout step k
    Just char -> Level.eventFromChar level char
    ) <$> getCharWithinDurationMicros durationMicros

getCharWithinDurationMicros :: Int -> IO (Maybe Char)
getCharWithinDurationMicros durationMicros =
  if durationMicros < 0
    then return Nothing
    else timeout durationMicros getChar
