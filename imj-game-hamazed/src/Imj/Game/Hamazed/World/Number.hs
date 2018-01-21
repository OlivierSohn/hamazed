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
import           Data.Map.Strict (Map, unions, fromList)
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.ParticleSystem.Design.Timing


destroyedNumbersParticleSystems :: (MonadState AppState m)
                                => Time Point ParticleSyst
                                -> Direction -- ^ 'Direction' of the laser shot
                                -> World -- ^ the 'World' the 'Number's live in
                                -> [Number]
                                -> m (Map ParticleSystemKey (Prioritized ParticleSystem))
destroyedNumbersParticleSystems keyTime dir world nums = do
  let laserSpeed = speed2vec $ coordsForDirection dir
  ps <- mapM (destroyedNumberParticleSystems keyTime laserSpeed world) nums
  return $ unions ps

destroyedNumberParticleSystems :: (MonadState AppState m)
                               => Time Point ParticleSyst
                               -> Vec2 Vel
                               -> World
                               -> Number
                               -> m (Map ParticleSystemKey (Prioritized ParticleSystem))
destroyedNumberParticleSystems k laserSpeed world (Number (PosSpeed pos _) n) = do
  envFuncs <- envFunctions world (WorldScope Air)
  keys <- forever takeKey
  return $ fromList
    $ zip keys
    $ map (Prioritized particleSystDefaultPriority)
    $ catMaybes [expandShrinkPolygon n pos cycleWallColors2 (Speed 1) envFuncs k]
     ++ fragmentsFreeFallThenExplode (scalarProd 0.8 laserSpeed) pos
          (\i -> if even i
                  then cycleOuterColors1
                  else cycleWallColors2)
          (intToDigit n) (Speed 2) envFuncs k
