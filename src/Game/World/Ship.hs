{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game.World.Ship
        ( shipAnims
        , createShipPos
        ) where

import           Imajuscule.Prelude

import           Data.Char( intToDigit )
import           Data.List( foldl' )
import           Data.Maybe( isNothing )

import           Animation

import           Game.World.Space
import           Game.Event

import           Geo.Discrete
import           Geo.Conversion

-- | If ship is colliding and not in "safe time", and the event is a gamestep,
--     creates an animation where the ship and the colliding number explode.
--
--   The ship animation will have the initial speed of the number and vice-versa,
--     to mimic the rebound due to the collision.
{-# INLINABLE shipAnims #-}
shipAnims :: (Draw e) => BattleShip  -> Event -> [BoundedAnimation e]
shipAnims (BattleShip (PosSpeed shipCoords shipSpeed) _ safeTime collisions) =
  \case
    Timeout GameStep k ->
      if not (null collisions) && isNothing safeTime
        then
          -- number and ship explode, they exchange speeds
          let collidingNumbersSpeed = foldl' sumCoords zeroCoords $ map (\(Number (PosSpeed _ speed) _) -> speed) collisions
              (Number _ n) = head collisions
          in  map ((`BoundedAnimation` WorldFrame) .
                    (\(char,f) -> mkAnimation f k SkipZero (Speed 1) (Just char))) $
                  map ((,) '|')            (explosion (speed2vec collidingNumbersSpeed) shipCoords) ++
                  map ((,) $ intToDigit n) (explosion (speed2vec shipSpeed) shipCoords)
        else
          []
    _ -> []


createShipPos :: Space -> [Number] -> IO PosSpeed
createShipPos space numbers = do
  let numPositions = map (\(Number (PosSpeed pos _) _) -> pos) numbers
  candidate@(PosSpeed pos _) <- createRandomPosSpeed space
  if pos `notElem` numPositions
    then
      return candidate
    else
      createShipPos space numbers
