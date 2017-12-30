{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Number(
    getColliding
  , computeActualLaserShot
  , destroyedNumbersAnimations
  ) where

import           Imj.Prelude

import           Data.Char( intToDigit )

import           Imj.Game.Hamazed.Loop.Event
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation
import           Imj.Timing

getColliding :: Coords Pos -> [Number] -> [Number]
getColliding pos =
  filter (\(Number (PosSpeed pos' _) _) -> pos == pos')

destroyedNumbersAnimations :: KeyTime
                           -> Direction
                           -- ^ 'Direction' of the laser shot
                           -> [Number]
                           -> [BoundedAnimation]
destroyedNumbersAnimations keyTime dir =
  let laserSpeed = speed2vec $ coordsForDirection dir
  in concatMap (destroyedNumberAnimations keyTime laserSpeed)

destroyedNumberAnimations :: KeyTime
                          -> Vec2 Vel
                          -> Number
                          -> [BoundedAnimation]
destroyedNumberAnimations keyTime laserSpeed (Number (PosSpeed pos _) n) =
  let char = intToDigit n
  in map (`BoundedAnimation` WorldFrame)
        $ animatedPolygon n pos keyTime (Speed 1) char
        : fragmentsFreeFallThenExplode (scalarProd 0.8 laserSpeed) pos keyTime (Speed 2) char
