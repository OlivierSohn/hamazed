{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Event
    ( Event(..)
    , userEventPriority
    , priority
    , eventFromKey
    , TimedEvent(..)
    , ActionTarget(..)
    , getKeyTime
    , coordsForActionTargets
    , DeadlineType(..)
    , Meta(..)
    ) where


import           Imj.Prelude

import           Data.List( foldl' )
import           Data.Maybe( mapMaybe )

import           Imj.Geo.Discrete
import           Imj.Key.Types
import           Imj.Timing

data TimedEvent = TimedEvent Event SystemTime

data Event =  Action ActionTarget Direction
            | Timeout DeadlineType KeyTime
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

data DeadlineType = GameDeadline
                  | AnimationDeadline
                  | MessageDeadline
                  | FrameAnimationDeadline
                  deriving(Eq, Show)

priority :: DeadlineType -> Int
priority FrameAnimationDeadline = -1
priority MessageDeadline        = 0
priority GameDeadline           = 1
-- userEvent is here (in terms of priority)
priority AnimationDeadline      = 3

userEventPriority :: Int
userEventPriority = 2

data ActionTarget = Ship
                  | Laser
                  deriving(Eq, Show)

eventFromKey :: Key -> Event
eventFromKey = \case
  Escape -> Interrupt Quit
  AlphaNum c -> case c of
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
    _   -> Nonsense
  _      -> Nonsense


getKeyTime :: Event -> Maybe KeyTime
getKeyTime (Timeout _ k) = Just k
getKeyTime _             = Nothing

coordsForActionTargets :: ActionTarget -> [Event] -> Coords
coordsForActionTargets target actions =
  foldl' sumCoords zeroCoords $ map coordsForDirection $ filterActions target actions

filterActions :: ActionTarget -> [Event] -> [Direction]
filterActions target =
  mapMaybe (maybeDirectionFor target)

maybeDirectionFor :: ActionTarget -> Event -> Maybe Direction
maybeDirectionFor targetFilter (Action actionTarget dir)
   | actionTarget == targetFilter = Just dir
   | otherwise                    = Nothing
maybeDirectionFor _ _ = Nothing
