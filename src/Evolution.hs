{-# LANGUAGE NoImplicitPrelude #-}

module Evolution
         ( evolve
         , evolve'
         , evolveIO
         , evolveDeltaTime
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
      lastFrame = Frame $ pred nSteps
  in Evolution from to lastFrame duration (discreteInvQuartEaseInOut nSteps)

data (DiscretelyInterpolable v)
   => Evolution v = Evolution {
    _evolutionFrom :: !v
  , _evolutionTo :: !v
  , _evolutionLastFrame :: !Frame
  , _evolutionDuration :: Float -- ^ Total duration in seconds
  , _evolutionInverseEase :: Float -> Float
}

evolveDeltaTime :: DiscretelyInterpolable v
                => Evolution v
                -> Frame
                -- ^ current frame
                -> Maybe Float
                -- ^ the time interval between this step and the next
evolveDeltaTime (Evolution _ _ lastFrame@(Frame lastStep) duration easeValToTime) frame@(Frame step)
  | frame < 0          = error "negative frame"
  | frame >= lastFrame = Nothing
  | otherwise          = Just dt
  where
    nextStep = succ step
    thisValue = fromIntegral step / fromIntegral lastStep
    targetValue = fromIntegral nextStep / fromIntegral lastStep
    dt = duration * (easeValToTime targetValue - easeValToTime thisValue)


{-# INLINABLE evolve #-} -- allow specialization
evolve :: DiscretelyInterpolable v
       => Evolution v
       -> Frame
       -- ^ current frame
       -> v
       -- ^ the value
evolve (Evolution from to lastFrame _ _) frame@(Frame step)
  | frame <= 0         = from
  | frame >= lastFrame = to
  | otherwise          = interpolate from to step

{-# INLINABLE evolve' #-} -- allow specialization
evolve' :: DiscretelyInterpolable v
        => Evolution v
        -> Frame
        -- ^ current frame
        -> w
        -- ^ the value
evolve' (Evolution from to _ _ _) (Frame step) =
  interpolate' from to $ assert (step >= 0) step


{-# INLINABLE evolveIO #-} -- allow specialization
evolveIO :: (DiscretelyInterpolable v)
         => Evolution v
         -> Frame
         -- ^ current frame
         -> IO ()
evolveIO (Evolution from to _ _ _) (Frame step) =
  interpolateIO from to $ assert (step >= 0) step
