{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Network.State
      ( mkWorldCreation
      , mkGameTiming
      ) where

import           Imj.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.World.Types

import           Imj.Game.Hamazed.Loop.Timing

mkWorldCreation :: WorldSpec -> WorldCreation
mkWorldCreation spec = WorldCreation (CreationAssigned Set.empty) (WorldId 0) spec Map.empty

mkGameTiming :: GameTiming
mkGameTiming = GameTiming Nothing initalGameMultiplicator
