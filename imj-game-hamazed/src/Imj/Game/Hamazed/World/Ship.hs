{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.World.Ship
        ( shipParticleSystems
        ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( foldl' )

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Geo.Discrete
import           Imj.Geo.Continuous
import           Imj.Graphics.ParticleSystem


{- | If the ship is colliding and not in "safe time", and the event is a gamestep,
this function creates an animation where the ship and the colliding number explode.

The ship 'ParticleSystem' will have the initial speed of the number and vice-versa,
to mimic the rebound due to the collision. -}
shipParticleSystems :: (MonadState AppState m)
                    => Time Point System
                    -> m [Prioritized ParticleSystem]
shipParticleSystems k =
  getWorld >>= \w -> do
    envFuncs <- envFunctions $ WorldScope Air
    let color i = if even i
                    then cycleOuterColors1
                    else cycleWallColors2
        sps (BattleShip _ _ _ Armored _) = return []
        sps (BattleShip _ _ _ Unarmored _) = return []
        sps (BattleShip _ _ _ Destroyed []) = return []
        sps (BattleShip _ (PosSpeed shipCoords shipSpeed) _ Destroyed collisions) = do
          -- when number and ship explode, they exchange speeds
          let collidingNumbersAvgSpeed =
                foldl' sumCoords zeroCoords
                $ map (\(Number (PosSpeed _ speed) _) -> speed) collisions
              numSpeed = scalarProd 0.4 $ speed2vec collidingNumbersAvgSpeed
              shipSpeed2 = scalarProd 0.4 $ speed2vec shipSpeed
              (Number _ n) = head collisions
              k' = systemTimePointToParticleSystemTimePoint k
          return
            $ map (Prioritized particleSystDefaultPriority)
            $ fragmentsFreeFallThenExplode numSpeed shipCoords color '|' (Speed 1) envFuncs k' ++
              fragmentsFreeFallThenExplode shipSpeed2 shipCoords color (intToDigit n) (Speed 1) envFuncs k'
    concat <$> mapM sps (getWorldShips w)
