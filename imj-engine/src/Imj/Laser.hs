{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Laser
    ( -- * Laser representations
    {- | 'LaserRay' and 'Ray' are parametrized by phantom types
    'Theoretical' and 'Actual' to indicate if the ray was computed taking
    obstacles into account or not:
     -}
      LaserRay(..)
    , Ray(..)
    , Theoretical
    , Actual
    -- ** Laser reach
    , LaserReach(..)
    -- ** Create a Theoretical Ray
    -- | 'shootLaser' and 'shootLaserWithOffset' create a 'Theoretical' ray, i.e it doesn't
    -- stop at obstacles:
    , shootLaser
    , shootLaserWithOffset
    -- ** Create an Actual Ray
    -- | 'computeActualLaserShot' converts a 'Theoretical' ray to an 'Actual' ray, i.e
    -- it stops at obstacles (or not), according to 'LaserPolicy':
    , LaserPolicy(..)
    , computeActualLaserShot
    -- * Utilities
    , afterEnd
    ) where

import           Imj.Prelude

import           Data.List( minimumBy, partition )
import           Data.Maybe( isJust, isNothing )

import           Imj.Draw
import           Imj.Geo.Discrete
import           Imj.Laser.Types
import           Imj.Physics.Discrete.Collision


-- | Same as 'shootLaser' but offsets the start 'Coords' by one in the shot 'Direction'.
shootLaserWithOffset :: Coords Pos
                     -- ^ Start coordinates
                     -> Direction
                     -- ^ Direction of the shot
                     -> LaserReach
                     -> (Coords Pos -> Location)
                     -- ^ Collision function
                     -> Maybe (Ray Theoretical)
shootLaserWithOffset shipCoords dir =
  shootLaser (translateInDir dir shipCoords) dir

-- | Creates a 'Ray' by extending from a 'Coords' until a collision is found.
shootLaser :: Coords Pos
           -> Direction
           -> LaserReach
           -> (Coords Pos -> Location)
           -> Maybe (Ray Theoretical)
shootLaser laserStart dir laserType getLocation =
  case getLocation laserStart of
    OutsideWorld -> Nothing
    InsideWorld ->
      case laserType of
        Infinite ->
          let continueExtension c = getLocation c == InsideWorld
              seg = mkSegmentByExtendingWhile laserStart dir continueExtension
          in Just $ Ray seg


stopRayAtFirstCollision :: [Coords Pos] -> Ray Theoretical -> (Ray Actual, Maybe (Coords Pos))
stopRayAtFirstCollision coords (Ray s) =
  let collisions =
        map (\(c, Just i) -> (c,i))
        $ filter (\(_, i) -> isJust i)
        $ zip coords
        $ map (`segmentContains` s) coords
      limitAtFirstCollision :: [(Coords Pos, Int)] -> Segment -> (Ray Actual, Maybe (Coords Pos))
      limitAtFirstCollision collis seg = case collis of
        [] -> (Ray seg, Nothing)
        l -> (Ray (changeSegmentLength (snd minElt) seg), Just $ fst minElt)
         where
           minElt = minimumBy (\(_, i) (_, j) -> compare (abs i) (abs j)) l
  in limitAtFirstCollision collisions s

-- | Returns the 'Coords' that is just after the end of the 'LaserRay'
afterEnd :: LaserRay Actual -> Coords Pos
afterEnd (LaserRay dir (Ray seg)) =
  translateInDir dir $ snd $ extremities seg

-- | Converts a 'Theoretical' laser ray to an 'Actual' one,
-- taking obstacles and a 'LaserPolicy' into account.
--
-- Returns a partition of obstacles between the remaining and the destroyed ones.
computeActualLaserShot :: [a]
                       -- ^ Obstacles.
                       -> (a -> Coords Pos)
                       -- ^ Obstacle to 'Coords' function.
                       -> LaserRay Theoretical
                       -- ^ The 'LaserRay' that doesn't take obstacles into account.
                       -> LaserPolicy
                       -> (([a],[a]), Maybe (LaserRay Actual))
computeActualLaserShot obstacles coords (LaserRay dir theoreticalRay@(Ray seg)) = \case
  DestroyAllObstacles  ->
    ( partition (\e -> isNothing $ segmentContains (coords e) seg) obstacles
    , Just $ LaserRay dir $ Ray seg)
  DestroyFirstObstacle ->
    let (rayActual, mayCoord) =
          stopRayAtFirstCollision (map coords obstacles) theoreticalRay
        remainingObstacles = case mayCoord of
          Nothing -> (obstacles,[])
          (Just pos') -> partition (\e -> coords e /= pos') obstacles
    in ( remainingObstacles
       , Just $ LaserRay dir rayActual)
