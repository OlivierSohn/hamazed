{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Number(
    Number(..)
  , showShotNumbers
  , getColliding
  , survivingNumbers
  ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

import           Data.Char( intToDigit )
import           Data.List( partition, foldl', length )
import           Data.Maybe( isNothing )
import           Data.Text(singleton, pack)

import           Color
import           Geo.Discrete
import           Laser( Ray(..)
                      , LaserRay(..)
                      , LaserPolicy(..)
                      , Theoretical
                      , Actual
                      , stopRayAtFirstCollision )
import           Render

data Number = Number {
    _numberPosSpeed :: !PosSpeed
  , _numberNum :: !Int
} deriving(Generic, Eq)

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
