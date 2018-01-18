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
animateUIPriority           = 70
continueMsgPriority         = 60
particleSystLaserPriority   = 55 -- so that the laser disappears promptly
moveItemsPriority           = 50
playerPriority              = 40
particleSystDefaultPriority = 30

data Prioritized a = Prioritized {
    _prioritizedPriority :: !Int
  , _prioritizedItem :: !a
}
