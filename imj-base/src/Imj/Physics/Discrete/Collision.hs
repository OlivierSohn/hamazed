{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Physics.Discrete.Collision
    ( -- * Handling collisions
    {- | To give a better realism in the game, if the location
    /before/ a collision is not touching a wall, then the position /after/ the
    collision will be forced to touch a wall. Note that forcing the position
    only happens if the absolute speed of one coordinate is >= 2.
    -}
      mirrorSpeedAndMoveToPrecollisionIfNeeded
    , CollisionStatus(..)
    , firstCollision
    , Location(..)
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete
import           Imj.Physics.Discrete.Types


-- | Describes if a collision exists.
data Location = InsideWorld
              -- ^ No collision.
              | OutsideWorld
              -- ^ A collision exists.
              deriving(Eq, Show)

data CollisionStatus = NoCollision
                     -- ^ no collision on the trajectory, position is unchanged
                     | PreCollision
                     -- ^ a collision exists on the trajectory,
                     -- position was changed to be just before the collision
                     -- and speed was mirrored

-- | On collision, mirrors speed and moves to the pre-collision position.
mirrorSpeedAndMoveToPrecollisionIfNeeded :: (Coords Pos -> Location)
               -- ^ Interaction function.
               -> PosSpeed
               -- ^ Input position and speed.
               -> (PosSpeed, CollisionStatus)
               -- ^ The speed was potentially mirrored
mirrorSpeedAndMoveToPrecollisionIfNeeded getLocation posspeed@(PosSpeed pos speed) =
  maybe
    (posspeed, NoCollision)
    adjustPosSpeed
    $ firstCollision getLocation trajectory
 where
  trajectory = bresenham $ mkSegment pos $ sumPosSpeed pos speed
  adjustPosSpeed (mirror, newPos) = (PosSpeed newPos $ mirrorSpeed speed mirror, PreCollision)

-- | Handles the first collision on a trajectory, assuming that the first position
-- has no collision.
firstCollision :: (Coords Pos -> Location)
               -- ^ The collision function.
               -> [Coords Pos]
               -- ^ The trajectory (the first position is expected to be collision-free).
               -> Maybe (Mirror, Coords Pos)
               -- ^ On collision, the kind of speed mirroring
               --   that should be applied and the position just before the collision.
firstCollision getLocation (p1:theRest@(p2:_)) =
  mirrorIfNeededAtomic getLocation (PosSpeed p1 (diffPosToSpeed p2 p1)) <|> firstCollision getLocation theRest
firstCollision _ _ = Nothing

-- | Mirrors a speed
mirrorSpeed :: Coords Vel -> Mirror -> Coords Vel
mirrorSpeed (Coords dr dc) m =
  case m of
    MirrorRow -> Coords (negate dr) dc
    MirrorCol -> Coords dr          (negate dc)
    MirrorAll -> Coords (negate dr) (negate dc)

-- | The kind of speed mirroring to apply in reaction to a collision.
data Mirror = MirrorRow
            -- ^ Mirror the y coordinate
            | MirrorCol
            -- ^ Mirror the x coordinate
            | MirrorAll
            -- ^ Mirror x and y coordinates

-- | When continuing with current speed, if at next iteration we encounter a wall
-- (or go through a wall for diagonal case),
-- we change the speed according to the normal of the closest wall before collision
mirrorIfNeededAtomic :: (Coords Pos -> Location) -> PosSpeed -> Maybe (Mirror, Coords Pos)
mirrorIfNeededAtomic getLocation (PosSpeed pos@(Coords r c) (Coords dr dc)) =
  let future = Coords (r+dr) (c+dc)
      isWall coord = getLocation coord == OutsideWorld
      mirror = case getLocation future of
        OutsideWorld
          | dr == 0   -> Just MirrorCol
          | dc == 0   -> Just MirrorRow
          | otherwise -> -- diagonal case
                case (isWall (Coords (r+dr) c),
                      isWall (Coords r (c+dc))) of
                        (True, True)   -> Just MirrorAll
                        (False, False) -> Just MirrorAll
                        (True, False)  -> Just MirrorRow
                        (False, True)  -> Just MirrorCol
        InsideWorld
          | dr == 0   -> Nothing
          | dc == 0   -> Nothing
          | otherwise -> -- diagonal case
                case (isWall (Coords (r+dr) c),
                      isWall (Coords r (c+dc))) of
                        (True, True)   -> Just MirrorAll
                        (False, False) -> Nothing
                        (True, False)  -> Just MirrorRow
                        (False, True)  -> Just MirrorCol
  in maybe Nothing (\m -> Just (m, pos)) mirror
