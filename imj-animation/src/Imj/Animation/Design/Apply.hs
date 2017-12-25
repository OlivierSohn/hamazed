{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Apply
    ( updateAnimatedPointsUpToDepth1
    ) where


import           Imj.Prelude

import           Data.List( length )
import           Data.Maybe( fromMaybe )

import           Imj.Animation.Design.Internal.Types
import           Imj.Geo.Discrete.Bresenham
import           Imj.Geo.Discrete
import           Imj.Iteration


-- | Updates the /depth 1/ animated points of an 'AnimatedPoints', using a single
-- geometric animation function.
updateAnimatedPointsUpToDepth1 :: (Coords -> Frame -> ([Coords], Maybe Char))
                               -- ^ Geometric animation for the level
                               -> Iteration
                               -> (Coords -> InteractionResult)
                               -- ^ Interaction function
                               -> AnimatedPoints
                               -> AnimatedPoints
updateAnimatedPointsUpToDepth1 animation iteration@(Iteration _ globalFrame) interaction (AnimatedPoints root startFrame branches onWall _) =
  let frame = globalFrame - startFrame
      (points, char) = animation root frame
      previousState = fromMaybe (replicate (length points) $ Right $ assert (interaction root == Stable) root) branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
      newBranches = combine points previousState iteration interaction onWall
  in AnimatedPoints root startFrame (Just newBranches) onWall char

combine :: [Coords]
        -> [Either AnimatedPoints Coords]
        -> Iteration
        -> (Coords -> InteractionResult)
        -> CanInteract
        -> [Either AnimatedPoints Coords]
combine points previousState iteration interaction onWall =
  zipWith
    (combinePoints interaction iteration onWall)
    points
    (assert (length previousState == length points) previousState)

combinePoints :: (Coords -> InteractionResult)
              -> Iteration
              -> CanInteract
              -> Coords
              -> Either AnimatedPoints Coords
              -> Either AnimatedPoints Coords
combinePoints interaction (Iteration _ frame) onWall point =
  either
    Left
    (\prevPoint' ->
      case onWall of
        Stop         -> error "animation should have stopped already"
        DontInteract -> Right point
        Interact nextOnWall ->
          -- The assert verifies that we can drop the first point of the trajectory.
          -- This is because the environment is static.
          let prevPoint = assert (interaction prevPoint' == Stable) prevPoint'
              trajectory = bresenham $ mkSegment prevPoint point
          in maybe
               (Right $ assert (interaction point == Stable) point)
               (\preCollision ->
                  Left $ AnimatedPoints preCollision frame Nothing nextOnWall Nothing)
               $ getCoordsBeforeMutation trajectory interaction
    )

-- The first point of the trajectory is expected to be stable
getCoordsBeforeMutation :: [Coords] -> (Coords -> InteractionResult) -> Maybe Coords
getCoordsBeforeMutation [] _ = error "not supposed to happen"
getCoordsBeforeMutation [_] _ = Nothing
getCoordsBeforeMutation (a:as@(b:_)) interaction =
  case interaction b of
    Stable -> getCoordsBeforeMutation as interaction
    Mutation -> Just a
