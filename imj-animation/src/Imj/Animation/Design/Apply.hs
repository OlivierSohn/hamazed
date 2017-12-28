{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Apply
    ( updatePointsAndMutateIfNeeded
    ) where


import           Imj.Prelude

import           Data.List( length )
import           Data.Maybe( fromMaybe )

import           Imj.Animation.Design.Types
import           Imj.Geo.Discrete.Bresenham
import           Imj.Geo.Discrete
import           Imj.Iteration


-- | Doesn't change the existing /level 1/ 'AnimatedPoints's, but can convert some
-- 'AnimatedPoint's to 'AnimatedPoints's.
updatePointsAndMutateIfNeeded :: (Coords -> Frame -> [AnimatedPoint])
                              -- ^ Geometric animation function
                              -> Coords
                              -- ^ Center of the animation
                              -> Frame
                              -- ^ Relative frame
                              -> (Coords -> InteractionResult)
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
        -> (Coords -> InteractionResult)
        -> [Either AnimatedPoints AnimatedPoint]
combine points previousState frame interaction =
  zipWith
    (combinePoints interaction frame)
    points
    (assert (length previousState == length points) previousState)

combinePoints :: (Coords -> InteractionResult)
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
