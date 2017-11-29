{-# LANGUAGE NoImplicitPrelude #-}

module Collision
    ( CollisionStatus(..)
    , firstCollision
    , mirrorIfNeeded
    ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types
import           Geo.Discrete( mkSegment , sumCoords, diffCoords )
import           Geo.Discrete.Bresenham(bresenham)
import           WorldSize( Location(..) )


-- | this datatype is awkward, TODO refactor
data CollisionStatus = NoCollision -- no collision on the trajectory, position is unchanged
                     | PreCollision -- a collision exists on the trajectory,
                                    -- position was changed to be just before the collision
                                    -- and speed was mirrored
-- Either:
--  - (collision found) mirrors speed and moves to the pre-collision position
--  - (no collision found) doesn't change anything
mirrorIfNeeded :: (Coords -> Location) -> PosSpeed -> (PosSpeed, CollisionStatus)
mirrorIfNeeded getLocation posspeed@(PosSpeed pos speed) =
  let trajectory = bresenham $ mkSegment pos $ sumCoords pos speed
      adjustPosSpeed (mirror, newPos) = (PosSpeed newPos $ mirrorCoords speed mirror, PreCollision)
  in maybe (posspeed, NoCollision) adjustPosSpeed $ firstCollision getLocation trajectory

-- | assumes the first position has no collision
firstCollision :: (Coords -> Location)
               -> [Coords]
               -- ^ the successive positions
               -> Maybe (Mirror, Coords)
               -- ^ Nothing if there is no collision, else the kind of speed mirroring
               --   that should be applied and the position just before the collision
firstCollision getLocation (p1:theRest@(p2:_)) =
  mirrorIfNeededAtomic getLocation (PosSpeed p1 (diffCoords p2 p1)) <|> firstCollision getLocation theRest
firstCollision _ _ = Nothing

mirrorCoords :: Coords -> Mirror -> Coords
mirrorCoords (Coords (Row dr) (Col dc)) m =
  case m of
    MirrorRow -> Coords (Row $ negate dr) (Col dc)
    MirrorCol -> Coords (Row dr)          (Col $ negate dc)
    MirrorAll -> Coords (Row $ negate dr) (Col $ negate dc)

data Mirror = MirrorRow | MirrorCol | MirrorAll

-- | When continuing with current speed, if at next iteration we encounter a wall
-- (or go through a wall for diagonal case),
-- we change the speed according to the normal of the closest wall before collision
mirrorIfNeededAtomic :: (Coords -> Location) -> PosSpeed -> Maybe (Mirror, Coords)
mirrorIfNeededAtomic getLocation (PosSpeed pos@(Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
  let future = Coords (Row $ r+dr) (Col $ c+dc)
      isWall coord = getLocation coord == OutsideWorld
      mirror = case getLocation future of
        OutsideWorld
          | dr == 0   -> Just MirrorCol
          | dc == 0   -> Just MirrorRow
          | otherwise -> -- diagonal case
                case (isWall (Coords (Row $ r+dr) (Col c)),
                      isWall (Coords (Row r) (Col $ c+dc))) of
                        (True, True)   -> Just MirrorAll
                        (False, False) -> Just MirrorAll
                        (True, False)  -> Just MirrorRow
                        (False, True)  -> Just MirrorCol
        InsideWorld
          | dr == 0   -> Nothing
          | dc == 0   -> Nothing
          | otherwise -> -- diagonal case
                case (isWall (Coords (Row $ r+dr) (Col c)),
                      isWall (Coords (Row r) (Col $ c+dc))) of
                        (True, True)   -> Just MirrorAll
                        (False, False) -> Nothing
                        (True, False)  -> Just MirrorRow
                        (False, True)  -> Just MirrorCol
  in maybe Nothing (\m -> Just (m, pos)) mirror
