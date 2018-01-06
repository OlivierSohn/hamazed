{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Ship
        ( shipAnims
        , createShipPos
        ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( foldl' )
import           Data.Maybe( isNothing )

import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.World.Space
import           Imj.Geo.Discrete
import           Imj.Geo.Continuous
import           Imj.Graphics.Animation

{- | If the ship is colliding and not in "safe time", and the event is a gamestep,
this function creates an animation where the ship and the colliding number explode.

The ship animation will have the initial speed of the number and vice-versa,
to mimic the rebound due to the collision.
-}
shipAnims :: World
          -> KeyTime
          -> [Animation]
shipAnims world@(World _ (BattleShip (PosSpeed shipCoords shipSpeed) _ safeTime collisions) _ _ _) k =
  if not (null collisions) && isNothing safeTime
    then
      -- when number and ship explode, they exchange speeds
      let collidingNumbersAvgSpeed = foldl' sumCoords zeroCoords $ map (\(Number (PosSpeed _ speed) _) -> speed) collisions
          numSpeed = scalarProd 0.4 $ speed2vec collidingNumbersAvgSpeed
          shipSpeed2 = scalarProd 0.4 $ speed2vec shipSpeed
          (Number _ n) = head collisions
          envFuncs = envFunctions world (WorldScope Air)
      in  fragmentsFreeFallThenExplode numSpeed shipCoords '|' (Speed 1) envFuncs (Right k)
          ++
          fragmentsFreeFallThenExplode shipSpeed2 shipCoords (intToDigit n) (Speed 1) envFuncs (Right k)
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
