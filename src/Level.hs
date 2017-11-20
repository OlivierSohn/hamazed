{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Level
    ( Level(..)
    , LevelFinished(..)
    , renderLevel
    , isLevelFinished
    , renderLevelState
    , MessageState(..)
    , GameStops(..)
    , firstLevel
    , lastLevel
    , messageDeadline
    , getEventForMaybeDeadline
    ) where

import           Imajuscule.Prelude

import           Control.Monad.Loops( unfoldM_ )

import           Data.Text( Text, pack )

import           System.IO( getChar )
import           System.Timeout( timeout )

import           Console( Color(..)
                        , ColorIntensity(..)
                        , setForeground
                        , restoreForeground
                        , renderTxt_ )
import           Deadline( Deadline(..) )
import           Event( Event(..)
                      , eventFromChar
                      , Step(..)
                      , TimedEvent(..) )
import           Geo( Direction(..) )
import           NonBlockingIO( tryGetChar )
import           Render( RenderState(..)
                       , move
                       , go )
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

data GameStops = Lost Text
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
            l  -> Just $ Lost $ "collision with " <> showListOrSingleton l)
          (const Nothing)
          safeTime
      _ -> Nothing -- this optimization is to not re-do the check when nothing has moved

    checkSum = case compare sumNumbers target of
      LT -> Nothing
      EQ -> Just Won
      GT -> Just $ Lost $ pack $ "sum " ++ show sumNumbers ++ " is bigger than target " ++ show target
    checkAmmo
      | ammo <= 0 = Just $ Lost $ pack "no ammo left"
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
    then
      return Nothing
    else
      timeout durationMicros (
        getChar >>=
          -- flush stdin to avoid repeated keys being used later
          (\c -> unfoldM_ tryGetChar >>
           return c))

renderLevelState :: RenderState -> Int -> LevelFinished -> IO ()
renderLevelState coords level (LevelFinished stop _ messageState) = do
  let color = case stop of
        (Lost _) -> Yellow
        Won      -> Green
      topLeft = go RIGHT coords
  prevFg <- setForeground Vivid color
  renderTxt_ (case stop of
    (Lost reason) -> "You Lose (" <> reason <> ")"
    Won           -> "You Win!") topLeft
  restoreForeground prevFg
  when (messageState == ContinueMessage) $
    renderTxt_ (if level == lastLevel
      then "You reached the end of the game! Hit Ctrl + C to quit."
      else
        let action = case stop of
                          (Lost _) -> "restart"
                          Won      -> "continue"
        in "Hit a key to " <> action <> " ...") (move 2 Down topLeft)


renderLevel :: Level -> RenderState -> IO ()
renderLevel (Level level levelState) rightMiddle =
  mapM_ (renderLevelState rightMiddle level) levelState
