{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World.Laser
    ( LaserType(..)
    , LaserRay(..)
    , LaserPolicy(..)
    , Ray(..)
    , afterEnd
    , Theoretical
    , Actual
    , stopRayAtFirstCollision
    , shootLaserFromShip
    ) where

import           Imj.Prelude

import           Data.List( minimumBy )
import           Data.Maybe( isJust )

import           Imj.Animation.Types
import           Imj.Draw
import           Imj.Geo.Discrete
import           Imj.Laser.Types
import           Imj.Physics.Discrete.Collision


shootLaserFromShip :: Coords
                   -> Direction
                   -> LaserType
                   -> (Coords -> Location)
                   -> Maybe (Ray Theoretical)
shootLaserFromShip shipCoords dir = shootLaser (translateInDir dir shipCoords) dir

shootLaser :: Coords
           -> Direction
           -> LaserType
           -> (Coords -> Location)
           -> Maybe (Ray Theoretical)
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


afterEnd :: LaserRay Actual -> Coords
afterEnd (LaserRay dir (Ray seg)) = translateInDir dir $ snd $ extremities seg
