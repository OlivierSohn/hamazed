{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Ship
        ( shipParticleSystems
        , createShipPos
        ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( foldl' )
import           Data.Maybe( isNothing )

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.World.Space
import           Imj.Geo.Discrete
import           Imj.Geo.Continuous
import           Imj.Graphics.ParticleSystem

{- | If the ship is colliding and not in "safe time", and the event is a gamestep,
this function creates an animation where the ship and the colliding number explode.

The ship 'ParticleSystem' will have the initial speed of the number and vice-versa,
to mimic the rebound due to the collision. -}
shipParticleSystems :: World
                    -> KeyTime
                    -> [ParticleSystem]
shipParticleSystems world@(World _ (BattleShip (PosSpeed shipCoords shipSpeed) _ safeTime collisions) _ _ _) k =
  if not (null collisions) && isNothing safeTime
    then
      -- when number and ship explode, they exchange speeds
      let collidingNumbersAvgSpeed = foldl' sumCoords zeroCoords $ map (\(Number (PosSpeed _ speed) _) -> speed) collisions
          numSpeed = scalarProd 0.4 $ speed2vec collidingNumbersAvgSpeed
          shipSpeed2 = scalarProd 0.4 $ speed2vec shipSpeed
          (Number _ n) = head collisions
          envFuncs = envFunctions world (WorldScope Air)
          color i = if even i
                      then cycleOuterColors1
                      else cycleWallColors2
      in  fragmentsFreeFallThenExplode numSpeed shipCoords color
            '|' (Speed 1) envFuncs (Right k)
          ++
          fragmentsFreeFallThenExplode shipSpeed2 shipCoords color
            (intToDigit n) (Speed 1) envFuncs (Right k)
    else
      []


createShipPos :: Space -> [Number] -> IO PosSpeed
createShipPos space numbers = do
  let numPositions = map (\(Number (PosSpeed pos _) _) -> pos) numbers
  candidate@(PosSpeed pos _) <- createRandomNonCollidingPosSpeed space
  if pos `notElem` numPositions
    then
      return candidate
    else
      createShipPos space numbers
