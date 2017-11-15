
module Laser
    ( LaserType(..)
    , laserChar
    , Ray(..)
    , Theoretical
    , Actual
    , stopRayAtFirstCollision
    , shootLaserFromShip
    ) where


import           Data.List( minimumBy )
import           Data.Maybe( isJust )
import           Geo( Coords(..)
                    , changeSegmentLength
                    , Direction(..)
                    , mkSegment
                    , Segment(..)
                    , segmentContains
                    , translateInDir )
import           Space( Space(..)
                      , getMaterial
                      , Material(..) )
import           World( extend )


data LaserType = Infinite
newtype Ray a = Ray Segment
data Theoretical -- with no obstacle
data Actual      -- with obstacles

shootLaserFromShip :: Coords -> Direction -> LaserType -> Space -> Maybe (Ray Theoretical)
shootLaserFromShip shipCoords dir = shootLaser (translateInDir dir shipCoords) dir

shootLaser :: Coords -> Direction -> LaserType -> Space -> Maybe (Ray Theoretical)
shootLaser laserStart dir laserType space =
  case getMaterial laserStart space of
    Wall -> Nothing
    Air ->
      case laserType of
        Infinite ->
          let laserEnd = extend laserStart dir space
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
  LEFT  -> '-'
  RIGHT -> '-'
