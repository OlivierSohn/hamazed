{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Number(
    getColliding
  , computeActualLaserShot
  , destroyedNumbersParticleSystems
  ) where

import           Imj.Prelude

import           Data.Char( intToDigit )

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.ParticleSystem


getColliding :: Coords Pos -> [Number] -> [Number]
getColliding pos =
  filter (\(Number (PosSpeed pos' _) _) -> pos == pos')

destroyedNumbersParticleSystems :: Either SystemTime KeyTime
                                -> Direction -- ^ 'Direction' of the laser shot
                                -> World -- ^ the 'World' the 'Number's live in
                                -> [Number]
                                -> [ParticleSystem]
destroyedNumbersParticleSystems keyTime dir world =
  let laserSpeed = speed2vec $ coordsForDirection dir
  in concatMap (destroyedNumberParticleSystems keyTime laserSpeed world)

destroyedNumberParticleSystems :: Either SystemTime KeyTime
                               -> Vec2 Vel
                               -> World
                               -> Number
                               -> [ParticleSystem]
destroyedNumberParticleSystems k laserSpeed world (Number (PosSpeed pos _) n) =
  let envFuncs = envFunctions world (WorldScope Air)
  in catMaybes [expandShrinkPolygon n pos cycleWallColors2 (Speed 1) envFuncs k]
     ++ fragmentsFreeFallThenExplode (scalarProd 0.8 laserSpeed) pos
          (\i -> if even i
                  then cycleOuterColors1
                  else cycleWallColors2)
          (intToDigit n) (Speed 2) envFuncs k
