
module Event
    ( Event(..)
    , TimedEvent(..)
    , ActionTarget(..)
    , getKeyTime
    , coordsForActionTargets
    , Step(..)
    ) where


import           Imajuscule.Prelude

import           Data.List( foldl' )

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
            | StartLevel Int
            | EndGame
            | Nonsense
            deriving(Eq, Show)

data Step = GameStep
          | AnimationStep
          | MessageStep
          deriving(Eq, Show)

data ActionTarget = Ship
                  | Laser
                  deriving(Eq, Show)


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
