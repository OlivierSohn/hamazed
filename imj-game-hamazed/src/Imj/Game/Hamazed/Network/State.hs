{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Network.State
      ( mkWorldCreation
      , mkGameTiming
      , mkCenterColor
      ) where

import           Imj.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.World.Types
import           Imj.Graphics.Color.Types

import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.Color

mkWorldCreation :: WorldSpec -> WorldCreation
mkWorldCreation spec = WorldCreation (CreationAssigned Set.empty) (WorldId 0) spec Map.empty

mkGameTiming :: GameTiming
mkGameTiming = GameTiming Nothing initalGameMultiplicator

mkCenterColor :: ColorScheme -> IO (Color8 Foreground)
mkCenterColor (ColorScheme c) = return c
mkCenterColor UseServerStartTime = do
  t <- getCurrentSecond
  let !ref = rgb 3 2 0
      nColors = countHuesOfSameIntensity ref
      n = t `mod` nColors
  return $ rotateHue (fromIntegral n / fromIntegral nColors) ref
