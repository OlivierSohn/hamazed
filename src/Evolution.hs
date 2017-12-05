{-# LANGUAGE NoImplicitPrelude #-}

module Evolution
         ( evolve
         , evolve'
         , evolveIO
         , evolveDeltaTime
         , mkEvolution
         , mkEvolution1
         , mkEvolution2
         , Evolution(..)
         -- reexports
         , module Interpolation
         ) where

import           Imajuscule.Prelude

import           Interpolation

import           Math


-- | for a more general version, see 'mkEvolution'
mkEvolution1 :: DiscretelyInterpolable v
            => v -- ^ from, to
            -> Float
            -- ^ duration in seconds
            -> Evolution v
mkEvolution1 fromto = mkEvolution (Successive [fromto])


-- | for a more general version, see 'mkEvolution'
mkEvolution2 :: DiscretelyInterpolable v
            => v -- ^ from
            -> v -- ^ to
            -> Float
            -- ^ duration in seconds
            -> Evolution v
mkEvolution2 from to = mkEvolution (Successive [from, to])


{-# INLINABLE mkEvolution #-} -- to allow specialization
mkEvolution :: DiscretelyInterpolable v
            => Successive v
            -> Float
            -- ^ duration in seconds
            -> Evolution v
mkEvolution s duration =
  let nSteps = distanceSuccessive s
      lastFrame = Frame $ pred nSteps
  in Evolution s lastFrame duration (discreteInvQuartEaseInOut nSteps)

-- TODO we could optimize by precomputing the lastframes of each individual segment,
-- and selecting the interval without having to recompute every distance.
-- We could change the Successive type to store the cumulated distance,
-- then do a binary search
data (DiscretelyInterpolable v) => Evolution v = Evolution {
    _evolutionSuccessive :: !(Successive v)
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
evolveDeltaTime (Evolution _ lastFrame@(Frame lastStep) duration easeValToTime) frame@(Frame step)
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
evolve (Evolution s@(Successive l) lastFrame _ _) frame@(Frame step)
  | frame <= 0         = head l
  | frame >= lastFrame = last l
  | otherwise          = interpolateSuccessive s step


{-# INLINABLE evolve' #-} -- allow specialization
evolve' :: DiscretelyInterpolable v
        => Evolution v
        -> Frame
        -- ^ current frame
        -> w
        -- ^ the value
evolve' (Evolution s _ _ _) (Frame step) =
  interpolateSuccessive' s $ assert (step >= 0) step


{-# INLINABLE evolveIO #-} -- allow specialization
evolveIO :: (DiscretelyInterpolable v)
         => Evolution v
         -> Frame
         -- ^ current frame
         -> IO ()
evolveIO (Evolution s _ _ _) (Frame step) =
  interpolateSuccessiveIO s $ assert (step >= 0) step
