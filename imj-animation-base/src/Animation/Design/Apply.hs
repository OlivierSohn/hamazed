{-# LANGUAGE NoImplicitPrelude #-}

-- | This module is about updating 'AnimatedPoints' using the pure animation function and
-- the collision function.

module Animation.Design.Apply
    (
      applyAnimation
    ) where


import           Imajuscule.Prelude

import           Data.List( length )
import           Data.Maybe( fromMaybe )

import           Animation.Types

import           Geo.Discrete.Bresenham
import           Geo.Discrete( mkSegment )
import           Physics.Discrete.Collision(firstCollision)


-- | Updates 'AnimatedPoints' using an animation function.
--
-- In particular, decides if a given animation point should continue to be "alive"
-- or if, due to a collision, it should now trigger an "evolution" at that point.
applyAnimation :: (Coords -> Frame -> ([Coords], Maybe Char))
               -- ^ Pure animation function
               -> Iteration
               -> (Coords -> Location)
               -- ^ Collision function
               -> AnimatedPoints
               -> AnimatedPoints
applyAnimation animation iteration@(Iteration _ globalFrame) getLocation (AnimatedPoints root startFrame branches onWall _) =
  let frame = globalFrame - startFrame
      (points, char) = animation root frame
      previousState = fromMaybe (replicate (length points) $ Right $ assert (getLocation root == InsideWorld) root) branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
      newBranches = combine points previousState iteration getLocation onWall
  in AnimatedPoints root startFrame (Just newBranches) onWall char

combine :: [Coords]
        -> [Either AnimatedPoints Coords]
        -> Iteration
        -> (Coords -> Location)
        -> CollisionReaction
        -> [Either AnimatedPoints Coords]
combine points previousState iteration getLocation onWall =
  zipWith
    (combinePoints getLocation iteration onWall)
    points
    (assert (length previousState == length points) previousState)

combinePoints :: (Coords -> Location)
              -> Iteration
              -> CollisionReaction
              -> Coords
              -> Either AnimatedPoints Coords
              -> Either AnimatedPoints Coords
combinePoints getLocation (Iteration _ frame) onWall point =
  either
    Left
    (\prevPoint ->
      case onWall of
        Stop               -> error "animation should have stopped already"
        Traverse           -> Right point
        ReboundAnd nextOnWall ->
                   let trajectory = bresenham (mkSegment (assert (getLocation prevPoint == InsideWorld) prevPoint) point)
                       collision = firstCollision getLocation trajectory
                   in  maybe
                         (Right $ assert (getLocation point == InsideWorld) point)
                         (\(_, preCollisionCoords) ->
                              Left $ AnimatedPoints preCollisionCoords frame Nothing nextOnWall Nothing
                         ) collision
    )
