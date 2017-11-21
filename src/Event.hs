{-# LANGUAGE NoImplicitPrelude #-}

module Event
    ( Event(..)
    , userEventPriority
    , priority
    , eventFromChar
    , TimedEvent(..)
    , ActionTarget(..)
    , getKeyTime
    , coordsForActionTargets
    , Step(..)
    , Meta(..)
    ) where


import           Imajuscule.Prelude

import           Data.List( foldl' )
import           Data.Char ( ord )
import           Data.Maybe( mapMaybe )

import           Geo( Coords(..)
                    , Direction(..)
                    , coordsForDirection
                    , sumCoords
                    , zeroCoords)
import           Timing( KeyTime
                       , UTCTime )

data TimedEvent = TimedEvent Event UTCTime

data Event =  Action ActionTarget Direction
            | Timeout Step KeyTime
            | Explosion Int
            | GravityExplosion
            | StartLevel Int
            | EndGame
            | Interrupt Meta
            | Nonsense
            deriving(Eq, Show)

data Meta = Configure
          | Quit
          | Help
          deriving(Eq, Show)

data Step = GameStep
          | AnimationStep
          | MessageStep
          deriving(Eq, Show)

priority :: Step -> Int
priority MessageStep = 0
priority GameStep    = 1
-- userEvent is here
priority AnimationStep = 3

userEventPriority :: Int
userEventPriority = 2

data ActionTarget = Ship
                  | Laser
                  deriving(Eq, Show)

eventFromChar :: Char -> Event
eventFromChar c = case c of
  'k' -> Action Laser Down
  'i' -> Action Laser Up
  'j' -> Action Laser LEFT
  'l' -> Action Laser RIGHT
  'd' -> Action Ship Down
  'e' -> Action Ship Up
  's' -> Action Ship LEFT
  'f' -> Action Ship RIGHT
  ' ' -> Explosion 2
  'g' -> GravityExplosion
  _   -> case ord c of
    27 {-esc-} -> Interrupt Quit
    _          -> Nonsense


getKeyTime :: Event -> Maybe KeyTime
getKeyTime (Timeout _ k) = Just k
getKeyTime _             = Nothing

coordsForActionTargets :: ActionTarget -> [Event] -> Coords
coordsForActionTargets target actions = foldl' sumCoords zeroCoords $ map coordsForDirection $ filterActions target actions

filterActions :: ActionTarget -> [Event] -> [Direction]
filterActions target = mapMaybe (maybeDirectionFor target)

maybeDirectionFor :: ActionTarget -> Event -> Maybe Direction
maybeDirectionFor targetFilter (Action actionTarget dir)
   | actionTarget == targetFilter = Just dir
   | otherwise                    = Nothing
maybeDirectionFor _ _ = Nothing
