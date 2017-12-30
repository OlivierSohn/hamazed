{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.Animation.Design.Update
    ( updateAnimationIfNeeded
    , earliestDeadline
    ) where


import           Imj.Prelude

import           Data.Either(partitionEithers)

import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Animation.Design.Timing
import           Imj.Iteration
import           Imj.Timing


{- | Updates the 'Animation' if the current time is /close enough/
(cf. 'animationUpdateMargin') to the 'Animation' deadline. Else, the unmodified
'Animation' is returned. -}
updateAnimationIfNeeded :: KeyTime
                        -- ^ The 'KeyTime' we want to update
                        -> Animation
                        -- ^ The current animation
                        -> Maybe Animation
                        -- ^ The updated animation, or Nothing if the 'Animation'
                        -- is over.
updateAnimationIfNeeded
 keyTime
 anim@(Animation points@(AnimatedPoints branches _ _) update interaction u@(UpdateSpec k it@(Iteration _ frame)) c) =
  let step = computeStep branches k keyTime
      newPoints = update interaction frame points
      newUpdateSpec = case step of
                        Update -> UpdateSpec (addDuration animationPeriod k) (nextIteration it)
                        _      -> u
      newAnim = Animation newPoints update interaction newUpdateSpec c
  in case step of
       Same -> Just anim
       _    -> case isActive newAnim of
                 True -> Just newAnim
                 False -> Nothing

defaultStep :: Maybe [Either AnimatedPoints AnimatedPoint] -> StepType
defaultStep =
  -- if branches is Nothing, it is the first time the animation is rendered / updated
  -- so we need to initialize the state
  maybe Initialize (const Same)

computeStep :: Maybe [Either AnimatedPoints AnimatedPoint]
            -- ^ The root branch.
            -> KeyTime
            -- ^ The animation 'KeyTime'
            -> KeyTime
            -- ^ The current 'KeyTime'
            -> StepType
computeStep mayBranches k keyTime =
  fromMaybe (defaultStep mayBranches) (computeStep' k keyTime)

computeStep' :: KeyTime
            -- ^ The animation 'KeyTime'
            -> KeyTime
            -- ^ The current 'KeyTime'
            -> Maybe StepType
computeStep' (KeyTime k') (KeyTime k) =
  -- group animations whose keytimes are close
  -- to reduce the amount of renderings needed
  if diffSystemTime k' k < animationUpdateMargin
    then
      Just Update
    else
      Nothing


isActive :: Animation
         -> Bool
isActive (Animation points _ _ _ _) =
  hasActivePoints points

hasActivePoints :: AnimatedPoints
                -> Bool
hasActivePoints (AnimatedPoints Nothing _ _)         = False
hasActivePoints (AnimatedPoints (Just []) _ _)       = False
hasActivePoints (AnimatedPoints (Just branches) _ _) =
  let (children, activeCoordinates) = partitionEithers branches
      childrenActive = map hasActivePoints children
  in (not . null) activeCoordinates || or childrenActive

-- | Returns the earliest animation deadline
earliestDeadline :: [Animation] -> Maybe KeyTime
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      let getDeadline (Animation _ _ _ (UpdateSpec k _) _) = k
      in Just $ minimum $ map getDeadline animations
