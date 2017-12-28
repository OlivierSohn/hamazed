{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Hamazed.World.Ship
        ( shipAnims
        , createShipPos
        ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( foldl' )
import           Data.Maybe( isNothing )

import           Imj.Animation
import           Imj.Geo.Discrete
import           Imj.Geo.Continuous
import           Imj.Hamazed.World.Space
import           Imj.Hamazed.World.Types
import           Imj.Hamazed.Event

{- | If the ship is colliding and not in "safe time", and the event is a gamestep,
this function creates an animation where the ship and the colliding number explode.

The ship animation will have the initial speed of the number and vice-versa,
to mimic the rebound due to the collision.
-}
shipAnims :: BattleShip
          -> Event
          -> [BoundedAnimation]
shipAnims (BattleShip (PosSpeed shipCoords shipSpeed) _ safeTime collisions) =
  \case
    Timeout (Deadline k MoveFlyingItems) ->
      if not (null collisions) && isNothing safeTime
        then
          -- when number and ship explode, they exchange speeds
          let collidingNumbersSpeed = foldl' sumCoords zeroCoords $ map (\(Number (PosSpeed _ speed) _) -> speed) collisions
              (Number _ n) = head collisions
          in  map (`BoundedAnimation` WorldFrame) $
                  fragmentsFreeFallThenExplode (scalarProd 0.4 $ speed2vec collidingNumbersSpeed) shipCoords k (Speed 1) '|'
                  ++
                  fragmentsFreeFallThenExplode (scalarProd 0.4 $ speed2vec shipSpeed) shipCoords k (Speed 1) (intToDigit n)
        else
          []
    _ -> []


createShipPos :: Space -> [Number] -> IO PosSpeed
createShipPos space numbers = do
  let numPositions = map (\(Number (PosSpeed pos _) _) -> pos) numbers
  candidate@(PosSpeed pos _) <- createRandomNonCollidingPosSpeed space
  if pos `notElem` numPositions
    then
      return candidate
    else
      createShipPos space numbers
