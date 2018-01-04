{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Physics.Discrete.Collision
    ( shouldMirrorAtomic
    , mirrorSpeedAndMoveToPrecollisionIfNeeded
    , CollisionStatus(..)
    , firstCollision
    , Location(..)
    , Mirror(..)
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

{- | If a collision would exist on the trajectory between the current position and the
next, mirrors speed and moves to the pre-collision position.
Else, does not change the position.

Note that the next position may be several pixels away from the current position,
depending on the speed.

If the location /before/ a collision is not touching a wall, then the position /after/ the
collision will be forced to touch a wall. Note that forcing the position
only happens if the absolute speed of one coordinate is >= 2.
-}
mirrorSpeedAndMoveToPrecollisionIfNeeded :: (Coords Pos -> Location)
                                         -- ^ Interaction function.
                                         -> PosSpeed
                                         -- ^ Input position and speed.
                                         -> (PosSpeed, CollisionStatus)
                                         -- ^ The position was maybe updated (depending on 'CollisionStatus')
                                         -- , the speed was potentially mirrored.
mirrorSpeedAndMoveToPrecollisionIfNeeded getLocation posspeed@(PosSpeed pos speed) =
  maybe
    (posspeed, NoCollision)
    adjustPosSpeed
    $ firstCollision getLocation trajectory
 where
  trajectory = bresenham $ mkSegment pos $ sumPosSpeed pos speed
  adjustPosSpeed (mirror, newPos) = (PosSpeed newPos $ mirrorSpeed speed mirror, PreCollision)

-- | Finds the first collision on a trajectory (assumes that the first position
-- has no collision).
firstCollision :: (Coords Pos -> Location)
               -- ^ The collision function.
               -> [Coords Pos]
               -- ^ The trajectory.
               -> Maybe (Mirror, Coords Pos)
               -- ^ On collision, returns the kind of speed mirroring
               --   that should be applied and the position just before the collision.
firstCollision getLocation (p1:theRest@(p2:_)) =
  let mayMirror = shouldMirrorAtomic getLocation p1 p2
  in maybe
      (firstCollision getLocation theRest)
      (\mirror -> Just (mirror, p1))
      mayMirror
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

{- | The two positions passed are expected to be consecutive in bresenham style,
i.e either next to one another or placed in diagonal (this is why the function
name contains /atomic/), else the behaviour is not guaranteed.

This function tells if the current speed should be mirrored, as a result of a
collision which could happen either on the /future/ position, or, when the
movement is in /diagonal/, on a /virtual/ position between current and future
positions (i.e a position that would /not/ have been part of the visible trajectory).

'Mirror' is deduced from the /normal/ of the closest wall before collision. -}
shouldMirrorAtomic :: (Coords Pos -> Location)
                     -> Coords Pos
                     -- ^ /Current/ position
                     -> Coords Pos
                     -- ^ /Future/ position
                     -> Maybe Mirror
                     -- ^ Nothing if speed should not be mirrored.
shouldMirrorAtomic getLocation (Coords r c) future@(Coords futR futC) =
  let isWall = (== OutsideWorld) . getLocation
  in case getLocation future of
        OutsideWorld
          | r == futR -> Just MirrorCol
          | c == futC -> Just MirrorRow
          | otherwise -> -- diagonal case
                case (isWall (Coords futR c),
                      isWall (Coords r    futC)) of
                        (True, True)   -> Just MirrorAll
                        (False, False) -> Just MirrorAll
                        (True, False)  -> Just MirrorRow
                        (False, True)  -> Just MirrorCol
        InsideWorld
          | r == futR -> Nothing
          | c == futC -> Nothing
          | otherwise -> -- diagonal case
                case (isWall (Coords futR c),
                      isWall (Coords r    futC)) of
                        (True, True)   -> Just MirrorAll
                        (False, False) -> Nothing
                        (True, False)  -> Just MirrorRow
                        (False, True)  -> Just MirrorCol
