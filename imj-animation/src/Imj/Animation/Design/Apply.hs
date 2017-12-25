{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Apply
    ( updateAnimatedPointsUpToLevel1
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
updateAnimatedPointsUpToLevel1 :: (Coords -> Frame -> [AnimatedPoint])
                               -- ^ Geometric animation for the level
                               -> Iteration
                               -> (Coords -> InteractionResult)
                               -- ^ Interaction function
                               -> AnimatedPoints
                               -> AnimatedPoints
updateAnimatedPointsUpToLevel1 animation iteration@(Iteration _ globalFrame) interaction (AnimatedPoints startFrame root branches) =
  let frame = globalFrame - startFrame
      points = animation (assert (interaction root == Stable) root) frame
      defaultState = map (\(AnimatedPoint canInteract _ _) -> Right $ AnimatedPoint canInteract root Nothing) points
      previousState = fromMaybe defaultState branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
      newBranches = combine points previousState iteration interaction
  in AnimatedPoints startFrame root (Just newBranches)

combine :: [AnimatedPoint]
        -> [Either AnimatedPoints AnimatedPoint]
        -> Iteration
        -> (Coords -> InteractionResult)
        -> [Either AnimatedPoints AnimatedPoint]
combine points previousState iteration interaction =
  zipWith
    (combinePoints interaction iteration)
    points
    (assert (length previousState == length points) previousState)

combinePoints :: (Coords -> InteractionResult)
              -> Iteration
              -> AnimatedPoint
              -> Either AnimatedPoints AnimatedPoint
              -> Either AnimatedPoints AnimatedPoint
combinePoints interaction (Iteration _ frame) point@(AnimatedPoint onWall coords _) =
  either
    Left
    (\(AnimatedPoint prevOnWall prevCoords' _) ->
      case assert (prevOnWall == onWall) onWall of
        DontInteract -> Right point
        Interact ->
          -- The assert verifies that we can drop the first point of the trajectory.
          -- This is because the environment is static.
          let prevCoords = assert (interaction prevCoords == Stable) prevCoords'
              trajectory = bresenham $ mkSegment prevCoords coords
          in maybe
               (Right $ assert (interaction coords == Stable) point)
               (\preCollision ->
                  Left $ AnimatedPoints frame preCollision Nothing)
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
