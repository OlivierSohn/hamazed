{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Loop.Event.Priorities
        ( playerPriority
        , animateUIPriority
        , continueMsgPriority
        , moveItemsPriority
        , particleSystDefaultPriority
        , particleSystLaserPriority
        , Prioritized(..)
        ) where

import           Imj.Prelude

{-# INLINE playerPriority #-}
{-# INLINE animateUIPriority #-}
{-# INLINE continueMsgPriority #-}
{-# INLINE moveItemsPriority #-}
{-# INLINE particleSystDefaultPriority #-}
{-# INLINE particleSystLaserPriority #-}
playerPriority, animateUIPriority, continueMsgPriority :: Int
moveItemsPriority, particleSystDefaultPriority, particleSystLaserPriority :: Int
playerPriority              = maxBound -- player is above all other so that when the game goes very fast,
                                       --   the player can still send commands promptly
animateUIPriority           = 60
continueMsgPriority         = 50
particleSystLaserPriority   = 45 -- so that the laser disappears promptly
moveItemsPriority           = 40
particleSystDefaultPriority = 30

data Prioritized a = Prioritized {
    _prioritizedPriority :: {-# UNPACK #-} !Int
  , _prioritizedItem :: !a
}
