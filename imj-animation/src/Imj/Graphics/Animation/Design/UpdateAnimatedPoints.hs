{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.Animation.Design.UpdateAnimatedPoints
    ( updateAnimatedPoints
    ) where


import           Imj.Prelude

import           Data.List( length )
import           Data.Maybe( fromMaybe )

import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Types
import           Imj.Iteration


{- | Given a length \( n \) list of animation functions, updates the \( n \)
first levels of an 'AnimatedPoints' using one animation function per level.

An 'AnimatedPoint' at level \( k <= n \) can mutate to an 'AnimatedPoints' by
interacting with its environment:

* if \( k = n \) , the new 'AnimatedPoints' will remain empty.
* if \( k < n \), the new 'AnimatedPoints' will be populated by 'AnimatedPoints'
using the \( k+1 \)th animation function.
-}
updateAnimatedPoints :: [Coords Pos -> Frame -> [AnimatedPoint]]
                     -- ^ The animation function at index @i@ updates
                     -- 'AnimatedPoints' at level @i@.
                     -> (Coords Pos -> InteractionResult)
                     -- ^ Interaction function
                     -> Frame
                     -- ^ Current iteration
                     -> AnimatedPoints
                     -> AnimatedPoints
updateAnimatedPoints [] _ _ aps = aps
updateAnimatedPoints (f:fs) interaction globalFrame (AnimatedPoints branches center startFrame) =
  let relativeFrame = globalFrame - startFrame
      branchesLevel1Mutated = updatePointsAndMutateIfNeeded f center relativeFrame interaction branches
      newBranches = map (\case
                            -- recurse for the 'AnimatedPoints's
                            Left aps -> Left $ updateAnimatedPoints fs interaction relativeFrame aps
                            -- the 'AnimatedPoint's are already up-to-date due to updatePointsAndMutateIfNeeded:
                            Right ap -> Right ap
                            ) branchesLevel1Mutated
  in AnimatedPoints (Just newBranches) center startFrame


-- | Doesn't change the existing /level 1/ 'AnimatedPoints's, but can convert some
-- 'AnimatedPoint's to 'AnimatedPoints's.
updatePointsAndMutateIfNeeded :: (Coords Pos -> Frame -> [AnimatedPoint])
                              -- ^ Geometric animation function
                              -> Coords Pos
                              -- ^ Center of the animation
                              -> Frame
                              -- ^ Relative frame
                              -> (Coords Pos -> InteractionResult)
                              -- ^ Interaction function
                              -> Maybe [Either AnimatedPoints AnimatedPoint]
                              -- ^ Current branches
                              -> [Either AnimatedPoints AnimatedPoint]
                              -- ^ Updated branches
updatePointsAndMutateIfNeeded animation root frame interaction branches =
  let points = animation (assert (interaction root == Stable) root) frame
      defaultState = map (\(AnimatedPoint canInteract _ _) -> Right $ AnimatedPoint canInteract root Nothing) points
      previousState = fromMaybe defaultState branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
  in combine points previousState frame interaction

combine :: [AnimatedPoint]
        -> [Either AnimatedPoints AnimatedPoint]
        -> Frame
        -> (Coords Pos -> InteractionResult)
        -> [Either AnimatedPoints AnimatedPoint]
combine points previousState frame interaction =
  let check = allowedPointCountVariation (length previousState) (length points)
  in  assert check $
      zipWith
        (combinePoints interaction frame)
        points previousState

combinePoints :: (Coords Pos -> InteractionResult)
              -> Frame
              -> AnimatedPoint
              -> Either AnimatedPoints AnimatedPoint
              -> Either AnimatedPoints AnimatedPoint
combinePoints interaction frame point@(AnimatedPoint onWall coords _) =
  either
    Left
    (\(AnimatedPoint prevOnWall prevCoords' _) ->
      case assert (prevOnWall == onWall) onWall of
        DontInteract -> Right point
        Interact ->
          -- The assert verifies that we can drop the first point of the trajectory.
          -- This is because the environment is static.
          let prevCoords = assert (interaction prevCoords' == Stable) prevCoords'
              trajectory = bresenham $ mkSegment prevCoords coords
          in maybe
               (Right $ assert (interaction coords == Stable) point)
               (\preCollision ->
                  Left $ AnimatedPoints Nothing preCollision frame)
               $ getCoordsBeforeMutation trajectory interaction
    )

-- The first point of the trajectory is expected to be stable
getCoordsBeforeMutation :: [Coords Pos] -> (Coords Pos -> InteractionResult) -> Maybe (Coords Pos)
getCoordsBeforeMutation [] _ = error "not supposed to happen"
getCoordsBeforeMutation [_] _ = Nothing
getCoordsBeforeMutation (a:as@(b:_)) interaction =
  case interaction b of
    Stable -> getCoordsBeforeMutation as interaction
    Mutation -> Just a

{- | Verifies that the variation in number of points is allowed:

Number of points generated by /animation functions/ should be always the same,
or change from non-zero to 0 (to indicate the end of the animation).
-}
allowedPointCountVariation :: Int
                           -- ^ From
                           -> Int
                           -- ^ To
                           -> Bool
allowedPointCountVariation from to =
  to == from || to == 0
