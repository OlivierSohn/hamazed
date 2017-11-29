{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Design.Apply
    (
      applyAnimation
    ) where


import           Imajuscule.Prelude

import           Data.List( length )
import           Data.Maybe( fromMaybe )

import           Animation.Types

import           Collision( firstCollision )

import           Game.World.Size( Location(..) )

import           Geo.Discrete.Bresenham
import           Geo.Discrete( mkSegment )


applyAnimation :: (Coords -> Frame -> ([Coords], Maybe Char))
               -> Iteration
               -> (Coords -> Location)
               -> Tree
               -> Tree
applyAnimation animation iteration@(Iteration (_,globalFrame)) getLocation (Tree root startFrame branches onWall _) =
  let frame = globalFrame - startFrame
      (points, char) = animation root frame
      previousState = fromMaybe (replicate (length points) $ Right $ assert (getLocation root == InsideWorld) root) branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
      newBranches = combine points previousState iteration getLocation onWall
  in Tree root startFrame (Just newBranches) onWall char

combine :: [Coords]
        -> [Either Tree Coords]
        -> Iteration
        -> (Coords -> Location)
        -> OnWall
        -> [Either Tree Coords]
combine points previousState iteration getLocation onWall =
  zipWith (combinePoints getLocation iteration onWall) points (assert (length previousState == length points) previousState)

combinePoints :: (Coords -> Location)
              -> Iteration
              -> OnWall
              -> Coords
              -> Either Tree Coords
              -> Either Tree Coords
combinePoints getLocation iteration onWall point =
  either Left (\prevPoint ->
    case onWall of
      Stop               -> error "animation should have stopped already"
      Traverse           -> Right point
      ReboundAnd nextOnWall ->
                 let trajectory = bresenham (mkSegment (assert (getLocation prevPoint == InsideWorld) prevPoint) point)
                     collision = firstCollision getLocation trajectory
                 in  maybe
                       (Right $ assert (getLocation point == InsideWorld) point)
                       (\(_, preCollisionCoords) ->
                            -- TODO use currentFrame instead of previous and verify combining animations look good:
                            -- using the previous was an historical choice when there was no notion of trajectory
                            -- but now, since here we move to the precollision, it makes sense to not skip a frame
                            -- anymore
                            let (Iteration(_,frame)) = previousIteration iteration
                            in Left $ Tree preCollisionCoords frame Nothing nextOnWall Nothing)
                                 collision)
