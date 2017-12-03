{-# LANGUAGE NoImplicitPrelude #-}

module Evolution
         ( evolve
         , mkEvolution
         , Evolution(..)
         -- reexports
         , module Interpolation
         ) where

import           Imajuscule.Prelude

import           Interpolation

import           Math


{-# INLINABLE mkEvolution #-} -- to allow specialization
mkEvolution :: DiscretelyInterpolable v
            => v
            -> v
            -> Float
            -- ^ duration in seconds
            -> Evolution v
mkEvolution from to duration =
  let nSteps = distance from to
  in Evolution from to (Frame (nSteps-1)) duration invQuartEaseInOut

data (DiscretelyInterpolable v)
   => Evolution v = Evolution {
    _evolutionFrom :: !v
  , _evolutionTo :: !v
  , _evolutionLastFrame :: !Frame
  , _evolutionDuration :: Float -- ^ Total duration in seconds
  , _evolutionInverseEase :: Float -> Float
}

{-# INLINABLE evolve #-} -- allow specialization
evolve :: DiscretelyInterpolable v
       => Evolution v
       -> Frame
       -- ^ current frame
       -> (v, Maybe Float)
       -- ^ the value, and maybe the time interval between this step and the next
evolve (Evolution from to lastFrame@(Frame lastStep) duration easeValToTime) frame@(Frame step)
  | frame >= lastFrame = (to, Nothing)
  | otherwise          = (interpolate from to $ assert (step >= 0) step, Just dt)
  where
    nextStep = succ step
    thisValue = fromIntegral step / fromIntegral lastStep
    targetValue = fromIntegral nextStep / fromIntegral lastStep
    dt = duration * (easeValToTime targetValue - easeValToTime thisValue)
