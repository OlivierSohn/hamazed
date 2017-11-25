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
import           Geo( Coords , bresenham , mkSegment )
import           WorldSize( Location(..) )


applyAnimation :: (Coords -> Frame -> [Coords])
               -> Iteration
               -> (Coords -> Location)
               -> Tree
               -> Tree
applyAnimation animation iteration@(Iteration (_,globalFrame)) getLocation (Tree root startFrame branches) =
  let frame = globalFrame - startFrame
      points = animation root frame
      previousState = fromMaybe (replicate (length points) $ Right $ assert (getLocation root == InsideWorld) root) branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
      newBranches = combine points previousState iteration getLocation
  in Tree root startFrame $ Just newBranches

combine :: [Coords]
        -> [Either Tree Coords]
        -> Iteration
        -> (Coords -> Location)
        -> [Either Tree Coords]
combine points uncheckedPreviousState iteration getLocation =
  let previousState = assert (length points == length uncheckedPreviousState) uncheckedPreviousState
  in zipWith (combinePoints getLocation iteration) points previousState

combinePoints :: (Coords -> Location)
              -> Iteration
              -> Coords
              -> Either Tree Coords
              -> Either Tree Coords
combinePoints getLocation iteration point =
  either Left (\prevPoint -> let trajectory = bresenham (mkSegment (assert (getLocation prevPoint == InsideWorld) prevPoint) point)
                                 collision =  firstCollision getLocation trajectory
                             in  maybe
                                   (Right $ assert (getLocation point == InsideWorld) point)
                                   (\(_, preCollisionCoords) ->
                                        -- TODO use currentFrame instead of previous and verify combining animations look good:
                                        -- using the previous was an historical choice when there was no notion of trajectory
                                        -- but now, since here we move to the precoliision, it makes sense to not skip a frame
                                        -- anymore
                                        let (Iteration(_,frame)) = previousIteration iteration
                                        in Left $ Tree preCollisionCoords frame Nothing)
                                   collision)
