{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.World.Number(
    getColliding
  , computeActualLaserShot
  , destroyedNumbersParticleSystems
  ) where

import           Imj.Prelude

import           Data.Char( intToDigit )

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.State
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.ParticleSystem

destroyedNumbersParticleSystems :: (MonadState AppState m)
                                => Either SystemTime KeyTime
                                -> Direction -- ^ 'Direction' of the laser shot
                                -> World -- ^ the 'World' the 'Number's live in
                                -> [Number]
                                -> m [ParticleSystem]
destroyedNumbersParticleSystems keyTime dir world nums = do
  let laserSpeed = speed2vec $ coordsForDirection dir
  ps <- mapM (destroyedNumberParticleSystems keyTime laserSpeed world) nums
  return $ concat ps

destroyedNumberParticleSystems :: (MonadState AppState m)
                               => Either SystemTime KeyTime
                               -> Vec2 Vel
                               -> World
                               -> Number
                               -> m [ParticleSystem]
destroyedNumberParticleSystems k laserSpeed world (Number (PosSpeed pos _) n) = do
  envFuncs <- envFunctions world (WorldScope Air)
  return $ catMaybes [expandShrinkPolygon n pos cycleWallColors2 (Speed 1) envFuncs k]
     ++ fragmentsFreeFallThenExplode (scalarProd 0.8 laserSpeed) pos
          (\i -> if even i
                  then cycleOuterColors1
                  else cycleWallColors2)
          (intToDigit n) (Speed 2) envFuncs k
