
module Level
    ( Level(..)
    , LevelFinished(..)
    , isLevelFinished
    , Level.eventFromChar
    , MessageState(..)
    , GameStops(..)
    , firstLevel
    , lastLevel
    ) where

import           Imajuscule.Prelude

import           Event( Event(..)
                      , eventFromChar
                      , Step(..)
                      , TimedEvent(..) )
import           Timing( UTCTime )
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
