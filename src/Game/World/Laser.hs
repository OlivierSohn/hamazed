{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Laser
    ( LaserType(..)
    , LaserRay(..)
    , LaserPolicy(..)
    , Ray(..)
    , afterEnd
    , Theoretical
    , Actual
    , stopRayAtFirstCollision
    , shootLaserFromShip
    , mkLaserAnimationUpdate
    ) where

import           Imajuscule.Prelude

import           Data.List( minimumBy )
import           Data.Maybe( isJust )

import           Animation.Types
import           Animation( mkAnimationUpdate
                          , simpleLaser)
import           Draw
import           Geo.Discrete

import           Game.World.Laser.Types

import           Timing( KeyTime )


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


-- no need to inline as we don't use e
mkLaserAnimationUpdate :: (Draw e) => KeyTime -> LaserRay Actual -> AnimationUpdate e
mkLaserAnimationUpdate keyTime ray@(LaserRay _ (Ray seg)) =
  let collisionFree = fst $ extremities seg -- this needs to be collision-free
  in mkAnimationUpdate (simpleLaser ray (mkAnimatedPoints collisionFree Traverse)) keyTime WithZero (Speed 1) Nothing


afterEnd :: LaserRay Actual -> Coords
afterEnd (LaserRay dir (Ray seg)) = translateInDir dir $ snd $ extremities seg
