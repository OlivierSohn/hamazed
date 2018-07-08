{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Priorities
        ( playerPriority
        , externalEventPriority
        , animateUIPriority
        , redrawStatusPriority
        , continueMsgPriority
        , moveItemsPriority
        , particleSystDefaultPriority
        , particleSystLaserPriority
        , Prioritized(..)
        ) where

import           Imj.Prelude

{-# INLINE playerPriority #-}
{-# INLINE animateUIPriority #-}
{-# INLINE redrawStatusPriority #-}
{-# INLINE continueMsgPriority #-}
{-# INLINE moveItemsPriority #-}
{-# INLINE particleSystDefaultPriority #-}
{-# INLINE particleSystLaserPriority #-}
playerPriority, animateUIPriority, redrawStatusPriority, continueMsgPriority :: Int
moveItemsPriority, particleSystDefaultPriority, particleSystLaserPriority :: Int
externalEventPriority :: Int
playerPriority              = maxBound -- player is above all other so that when the game goes very fast,
                                       --   the player can still send commands promptly
externalEventPriority       = 0 --maxBound -- TODO restore
animateUIPriority           = 60
continueMsgPriority         = 50
particleSystLaserPriority   = 45 -- so that the laser disappears promptly
moveItemsPriority           = 40
particleSystDefaultPriority = 30
redrawStatusPriority        = 20

data Prioritized a = Prioritized {
    _prioritizedPriority :: {-# UNPACK #-} !Int
  , _prioritizedItem :: !a
}
