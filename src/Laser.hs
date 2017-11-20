{-# LANGUAGE NoImplicitPrelude #-}

module Laser
    ( LaserType(..)
    , LaserRay(..)
    , LaserPolicy(..)
    , laserChar
    , Ray(..)
    , Theoretical
    , Actual
    , stopRayAtFirstCollision
    , shootLaserFromShip
    , mkLaserAnimation
    ) where

import           Imajuscule.Prelude

import           Data.List( minimumBy )
import           Data.Maybe( isJust )

import           Animation( Animation
                          , mkAnimation
                          , simpleLaser)
import           Geo( Coords(..)
                    , changeSegmentLength
                    , Direction(..)
                    , extend
                    , mkSegment
                    , Segment(..)
                    , segmentContains
                    , translateInDir )
import           Timing( KeyTime )
import           WorldSize( Location(..) )


data LaserRay a = LaserRay {
    _laserRayDir :: !Direction
  , _laserRaySeg :: !(Ray a)
}

data LaserPolicy = RayDestroysFirst | RayDestroysAll

data LaserType = Infinite

newtype Ray a = Ray Segment
data Theoretical -- with no obstacle
data Actual      -- with obstacles

shootLaserFromShip :: Coords -> Direction -> LaserType -> (Coords -> Location) -> Maybe (Ray Theoretical)
shootLaserFromShip shipCoords dir = shootLaser (translateInDir dir shipCoords) dir

shootLaser :: Coords -> Direction -> LaserType -> (Coords -> Location) -> Maybe (Ray Theoretical)
shootLaser laserStart dir laserType getLocation =
  case getLocation laserStart of
    OutsideWorld -> Nothing
    InsideWorld ->
      case laserType of
        Infinite ->
          let continueExtension c = getLocation c == InsideWorld
              laserEnd = extend laserStart dir continueExtension
          in Just $ Ray $ mkSegment laserStart laserEnd


stopRayAtFirstCollision :: [Coords] -> Ray Theoretical -> (Ray Actual, Maybe Coords)
stopRayAtFirstCollision coords (Ray s) =
  let collisions = map (\(c, Just i) -> (c,i)) $ filter (\(_, i) -> isJust i) $ zip coords $ map (`segmentContains` s) coords
      limitAtFirstCollision :: [(Coords, Int)] -> Segment -> (Ray Actual, Maybe Coords)
      limitAtFirstCollision collis seg = case collis of
        [] -> (Ray seg, Nothing)
        l -> (Ray (changeSegmentLength (snd minElt) seg), Just $ fst minElt)
         where
           minElt = minimumBy (\(_, i) (_, j) -> compare (abs i) (abs j)) l
  in limitAtFirstCollision collisions s


laserChar :: Direction -> Char
laserChar dir = case dir of
  Up    -> '|'
  Down  -> '|'
  LEFT  -> '='
  RIGHT -> '='

mkLaserAnimation :: KeyTime -> LaserRay a -> Animation
mkLaserAnimation keyTime (LaserRay laserDir (Ray laserSeg)) =
  mkAnimation (simpleLaser laserSeg (laserChar laserDir)) keyTime
