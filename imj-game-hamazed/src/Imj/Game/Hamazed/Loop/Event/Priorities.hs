{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Loop.Event.Priorities
        ( deadlinePriority
        , playerEventPriority
        ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Loop.Event.Types

playerEventPriority :: Int
playerEventPriority = 40

-- Note that if changing priorities here you should also change 'getDeadlinesByDecreasingPriority'
deadlinePriority :: DeadlineType -> Int
deadlinePriority AnimateUI              = playerEventPriority + 30
deadlinePriority DisplayContinueMessage = playerEventPriority + 20
deadlinePriority MoveFlyingItems        = playerEventPriority + 10
deadlinePriority AnimateParticleSystems = playerEventPriority - 10
