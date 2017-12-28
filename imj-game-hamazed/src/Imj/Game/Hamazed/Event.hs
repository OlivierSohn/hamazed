{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Event
    ( TimestampedEvent(..)
    , Event(..)
    , Deadline(..)
    , ActionTarget(..)
    , playerEventPriority
    , deadlinePriority
    , eventFromKey
    , getKeyTime
    , DeadlineType(..)
    , MetaAction(..)
    -- * Reexports (for haddock hyperlinks)
    , module Imj.Game.Hamazed.World.Types
    , module Imj.Graphics.Animation.Design.Animation
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.World.Types
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Animation
import           Imj.Input.Types
import           Imj.Timing

-- | A foreseen game or animation update.
data Deadline = Deadline {
    _deadlineTime :: !KeyTime
  , _deadlineType :: !DeadlineType
} deriving(Eq, Show)


data TimestampedEvent = TimestampedEvent {
    _timestampedEventEvt :: !Event
  , _timestampedEventTime :: !SystemTime
    -- ^ When the 'Event' occured.
} deriving(Show)

data Event = Action !ActionTarget !Direction
           -- ^ A player action on an 'ActionTarget' in a 'Direction'.
           | Timeout !Deadline
           -- ^ The 'Deadline' that needs to be handled immediately.
           | StartLevel !Int
           -- ^ New level.
           | EndGame
           -- ^ End of game.
           | Interrupt !MetaAction
           -- ^ A game interruption.
           deriving(Eq, Show)

data MetaAction = Quit
                -- ^ The player decided to quit the game.
                | Configure
                -- ^ The player wants to configure the game /(Not implemented yet)/
                | Help
                -- ^ The player wants to read the help page /(Not implemented yet)/
                deriving(Eq, Show)

data DeadlineType = MoveFlyingItems
                  -- ^ Move 'Number's and 'BattleShip' according to their current
                  -- speeds.
                  | Animate
                  -- ^ Update one or more 'Animation's.
                  | DisplayContinueMessage
                  -- ^ Show the /Hit a key to continue/ message
                  | AnimateUI
                  -- ^ Update the inter-level animation
                  deriving(Eq, Show)

playerEventPriority :: Int
playerEventPriority = 40

-- Note that if changing priorities here you should also change 'getDeadlinesByDecreasingPriority'
deadlinePriority :: DeadlineType -> Int
deadlinePriority AnimateUI              = playerEventPriority + 30
deadlinePriority DisplayContinueMessage = playerEventPriority + 20
deadlinePriority MoveFlyingItems        = playerEventPriority + 10
deadlinePriority Animate                = playerEventPriority - 10

data ActionTarget = Ship
                  -- ^ The player wants to accelerate the 'BattleShip'
                  | Laser
                  -- ^ The player wants to shoot with the laser.
                  deriving(Eq, Show)

-- | Tries to map a 'Key' (pressed by the player) to an 'Event'.
eventFromKey :: Key -> Maybe Event
eventFromKey = \case
  Escape -> Just $ Interrupt Quit
  AlphaNum c -> case c of
    'k' -> Just $ Action Laser Down
    'i' -> Just $ Action Laser Up
    'j' -> Just $ Action Laser LEFT
    'l' -> Just $ Action Laser RIGHT
    'd' -> Just $ Action Ship Down
    'e' -> Just $ Action Ship Up
    's' -> Just $ Action Ship LEFT
    'f' -> Just $ Action Ship RIGHT
    _   -> Nothing
  _ -> Nothing


getKeyTime :: Event -> Maybe KeyTime
getKeyTime (Timeout (Deadline k _)) = Just k
getKeyTime _                        = Nothing
