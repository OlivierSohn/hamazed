{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.GameItem.Weapon.Laser
    ( -- * Laser representations
    {- | 'LaserRay' is parametrized by phantom types
    'Theoretical' and 'Actual' to indicate if the ray was computed taking
    obstacles into account or not:
     -}
      LaserRay(..)
    , Theoretical
    , Actual
    -- ** Laser reach
    , LaserReach(..)
    -- ** Create a Theoretical LaserRay
    -- | 'shootLaser' and 'shootLaserWithOffset' create a 'Theoretical' ray, i.e it doesn't
    -- stop at obstacles:
    , shootLaser
    , shootLaserWithOffset
    -- ** Create an Actual LaserRay
    -- | 'computeActualLaserShot' converts a 'Theoretical' ray to an 'Actual' ray, i.e
    -- it stops at obstacles (or not), according to 'LaserPolicy':
    , LaserPolicy(..)
    , computeActualLaserShot
    -- * Utilities
    , afterEnd
    , visibleSegment
    ) where

import           Imj.Prelude

import           Data.Word(Word32)
import           Data.List( minimumBy )
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(partition, elems, empty)
import           Data.Maybe( isJust, isNothing )

import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable

import           Imj.GameItem.Weapon.Laser.Types
import           Imj.Physics.Discrete.Collision


-- | Same as 'shootLaser' but offsets the start 'Coords' by one in the shot 'Direction'.
shootLaserWithOffset :: Coords Pos
                     -- ^ Start coordinates
                     -> Direction
                     -- ^ Direction of the shot
                     -> LaserReach
                     -> (Coords Pos -> Location)
                     -- ^ Collision function
                     -> LaserRay Theoretical
shootLaserWithOffset shipCoords dir =
  shootLaser (translateInDir dir shipCoords) dir

-- | Creates a 'LaserRay' by extending from a 'Coords' until a collision is found.
shootLaser :: Coords Pos
           -> Direction
           -> LaserReach
           -> (Coords Pos -> Location)
           -> LaserRay Theoretical
shootLaser start dir laserType getLocation =
  case laserType of
    Infinite ->
      LaserRay dir start $ countInDir dir start ((== InsideWorld) . getLocation)

stopRayAtFirstCollision :: [Coords Pos]
                        -> LaserRay Theoretical
                        -> (LaserRay Actual, Maybe (Coords Pos))
stopRayAtFirstCollision coords s@(LaserRay dir start len) =
  let collisions :: [(Coords Pos, Word32)]
      collisions =
        map (\(c, Just indexHit) -> (c,indexHit))
        $ filter (\(_, indexHit) -> isJust indexHit)
        $ zip coords
        $ map (`laserHits` s) coords
      (newLen, res) = case collisions of
        [] -> (len, Nothing)
        l -> let minElt = minimumBy (\(_, i) (_, j) -> compare i j) l
             in (succ $ snd minElt, Just $ fst minElt)
  in (LaserRay dir start newLen, res)

-- | Returns the 'Coords' that is just after the end of the 'LaserRay'
afterEnd :: LaserRay Actual -> Coords Pos
afterEnd (LaserRay dir start len) =
  move (fromIntegral len) dir start

-- | Converts a 'Theoretical' laser ray to an 'Actual' one,
-- taking obstacles and a 'LaserPolicy' into account.
--
-- Returns a partition of obstacles between the remaining and the destroyed ones.
computeActualLaserShot :: Map k v
                       -- ^ Obstacles.
                       -> (v -> Coords Pos)
                       -- ^ Obstacle to 'Coords' function.
                       -> LaserRay Theoretical
                       -- ^ The 'LaserRay' that doesn't take obstacles into account.
                       -> LaserPolicy
                       -> ((Map k v, Map k v), LaserRay Actual)
computeActualLaserShot obstacles coords th@(LaserRay dir startTheoretical lenTheoretical) = \case
  DestroyAllObstacles  ->
    ( Map.partition (\e -> isNothing $ laserHits (coords e) th) obstacles
    , LaserRay dir startTheoretical lenTheoretical)
  DestroyFirstObstacle ->
    let (actual, mayCoord) =
          stopRayAtFirstCollision (map coords $ Map.elems obstacles) th
        remainingObstacles = case mayCoord of
          Nothing -> (obstacles,Map.empty)
          (Just pos') -> Map.partition (\e -> coords e /= pos') obstacles
    in ( remainingObstacles
       , actual)

visibleSegment :: LaserRay a -> Maybe Segment
visibleSegment (LaserRay dir start len)
  | len == 0  = Nothing
  | otherwise = Just $ mkSegment start (move (fromIntegral $ pred len) dir start)

-- | Returns the index, from laser start, at which the obstacle was hit.
laserHits :: Coords Pos -> LaserRay a -> Maybe Word32
laserHits (Coords r c) (LaserRay dir start@(Coords startR startC) len)
  | len == 0     = Nothing
  | r == startR  = isBetween c startC endC
  | c == startC  = isBetween r startR endR
  | otherwise    = Nothing
 where
  (Coords endR endC) = move (fromIntegral $ pred len) dir start
  isBetween x b b'
    | x <= b && x >= b' = Just $ fromIntegral (b-x)
    | x <= b' && x >= b = Just $ fromIntegral (x-b)
    | otherwise = Nothing
