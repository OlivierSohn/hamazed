{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Hamazed.World.Number(
    getColliding
  , computeActualLaserShot
  , destroyedNumbersAnimations
  ) where

import           Imj.Prelude

import           Imj.Animation

import           Data.Char( intToDigit )

import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Hamazed.World.Types
import           Imj.Hamazed.Event
import           Imj.Laser
import           Imj.Timing

getColliding :: Coords -> [Number] -> [Number]
getColliding pos = filter (\(Number (PosSpeed pos' _) _) -> pos == pos')

destroyedNumbersAnimations :: KeyTime
                           -> Event
                           -> [Number]
                           -> [BoundedAnimation]
destroyedNumbersAnimations keyTime event =
  let laserSpeed = case event of
        (Action Laser dir) -> speed2vec $ coordsForDirection dir
        _                  -> Vec2 0 0
  in concatMap (destroyedNumberAnimations keyTime laserSpeed)

destroyedNumberAnimations :: KeyTime
                          -> Vec2
                          -> Number
                          -> [BoundedAnimation]
destroyedNumberAnimations keyTime laserSpeed (Number (PosSpeed pos _) n) =
  let char = intToDigit n
  in map (`BoundedAnimation` WorldFrame)
        $ animatedPolygon n pos keyTime (Speed 1) char
        : fragmentsFreeFallThenExplode (scalarProd 2 laserSpeed) pos keyTime (Speed 2) char
