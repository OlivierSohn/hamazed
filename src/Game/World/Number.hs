{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game.World.Number(
    showShotNumbers
  , getColliding
  , survivingNumbers
  , destroyedNumbersAnimations
  ) where

import           Imajuscule.Prelude

import           Animation

import           Data.Char( intToDigit )
import           Data.List( partition, foldl', length )
import           Data.Maybe( isNothing )
import           Data.Text(singleton, pack)

import           Color

import           Game.World.Types
import           Game.World.Laser
import           Game.Event

import           Geo.Conversion
import           Geo.Continuous
import           Geo.Discrete

import           Timing

import           Render

getColliding :: Coords -> [Number] -> [Number]
getColliding pos = filter (\(Number (PosSpeed pos' _) _) -> pos == pos')

survivingNumbers :: [Number] -> LaserPolicy -> LaserRay Theoretical -> (([Number],[Number]), Maybe (LaserRay Actual))
survivingNumbers l policy (LaserRay dir theoreticalRay@(Ray seg)) = case policy of
  RayDestroysAll   -> (partition (\(Number (PosSpeed pos _) _) -> (isNothing $ segmentContains pos seg)) l, justFull)
  RayDestroysFirst ->
    let (rayActual, mayCoord) = stopRayAtFirstCollision (map (\(Number (PosSpeed pos _) _) -> pos) l) theoreticalRay
        remainingNumbers = case mayCoord of
          Nothing -> (l,[])
          (Just pos') -> partition (\(Number (PosSpeed pos _) _) -> pos /= pos') l
    in (remainingNumbers, Just $ LaserRay dir rayActual)
 where
   justFull = Just $ LaserRay dir $ Ray seg


showShotNumbers :: [Int] -> ColorString
showShotNumbers nums =
  let lastIndex = length nums - 1
      first = colored (singleton '[') bracketsColor
      last_ = colored (singleton ']') bracketsColor
      middle = snd $ foldl' (\(i,s) n -> let num = intToDigit n
                                             t = case i of
                                                  0 -> singleton num
                                                  _ -> pack [num, ' ']
                                         in (i-1, s <> colored t (numberColor n))) (lastIndex, first) nums

  in middle <> last_


destroyedNumbersAnimations :: KeyTime -> Event -> [Number] -> [BoundedAnimation]
destroyedNumbersAnimations keyTime event =
  let sp = case event of
        (Action Laser dir) -> speed2vec $ coordsForDirection dir
        _                  -> Vec2 0 0
      animation pos = map (\f -> (f, Speed 2)) (explosion (scalarProd 2 sp) pos)
  in \case
        Number (PosSpeed pos _) n:_ ->
          let animations = animation pos ++ [(animatedNumber n (mkAnimationTree pos Traverse), Speed 1)]
              create (f,speed) = mkAnimation f keyTime SkipZero speed $ Just $ intToDigit n
          in  map (\a -> BoundedAnimation (create a) WorldFrame) animations
        _ -> []
