module Number(
    Number(..)
  , getColliding
  , survivingNumbers
  ) where

import           Imajuscule.Prelude

import           Data.List( partition )

import           Data.Maybe( isNothing )

import           Geo( Coords(..)
                    , PosSpeed(..)
                    , segmentContains )
import           Laser( Ray(..)
                      , LaserRay(..)
                      , LaserPolicy(..)
                      , Theoretical
                      , Actual
                      , stopRayAtFirstCollision
                      )

data Number = Number {
    _numberPosSpeed :: !PosSpeed
  , _numberNum :: !Int
}

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
