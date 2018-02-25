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

import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.ParticleSystem.Design.Timing


destroyedNumbersParticleSystems :: (MonadState AppState m)
                                => Time Point ParticleSyst
                                -> ShipId
                                -> Direction -- ^ 'Direction' of the laser shot
                                -> [Number]
                                -> m [Prioritized ParticleSystem]
destroyedNumbersParticleSystems keyTime shipId dir nums = do
  let laserSpeed = speed2vec $ coordsForDirection dir
  ps <- mapM (destroyedNumberParticleSystems keyTime shipId laserSpeed) nums
  return $ concat ps

destroyedNumberParticleSystems :: (MonadState AppState m)
                               => Time Point ParticleSyst
                               -> ShipId
                               -> Vec2 Vel
                               -> Number
                               -> m [Prioritized ParticleSystem]
destroyedNumberParticleSystems k shipId laserSpeed (Number (PosSpeed pos _) n) = getPlayer shipId >>= maybe
  (return [])
  (\(Player _ _ (PlayerColors _ cycles)) -> do
    envFuncs <- envFunctions (WorldScope Air)
    let color i = cycleColors sumFrameParticleIndex $
                    if even i
                      then outer1 cycles
                      else wall2 cycles
        color' = cycleColors sumFrameParticleIndex $ wall2 cycles
    return
      $ map (Prioritized particleSystDefaultPriority)
      $ catMaybes [expandShrinkPolygon n pos color' (Speed 1) envFuncs k]
       ++ fragmentsFreeFallThenExplode (scalarProd 0.8 laserSpeed) pos color (intToDigit n) (Speed 2) envFuncs k)
