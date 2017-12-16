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
         , mkEaseClock
         , EaseClock(..)
         -- reexports
         , module Interpolation
         ) where

import           GHC.Show(showString)

import           Imajuscule.Prelude

import           Render.Draw
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

-- | Relies on a "fake" evolution that should be used for synchronization only, i.e
--   'evolveDeltaTime' can be used on it but not 'evolve'
newtype EaseClock = EaseClock (Evolution NotDiscretelyInterpolable) deriving (Show)
newtype NotDiscretelyInterpolable = NotDiscretelyInterpolable () deriving(Show)

instance DiscretelyInterpolable NotDiscretelyInterpolable where
  distance = error "don't use distance on NotDiscretelyInterpolable"
  interpolate = error "don't use interpolate on NotDiscretelyInterpolable"

mkEaseClock :: Float
            -- ^ duration in seconds
            -> Frame
            -- ^ last frame
            -> (Float -> Float)
            -- ^ ease function
            -> EaseClock
mkEaseClock duration lastFrame ease =
  let nSteps = fromIntegral $ succ lastFrame
  in EaseClock $ Evolution (Successive []) lastFrame duration (discreteAdaptor ease nSteps)

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

instance (DiscretelyInterpolable v, Show v) => Show (Evolution v) where
        showsPrec _ (Evolution a b c _) = showString $ "Evolution{" ++ show a ++ show b ++ show c ++ "}"

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


{-# INLINABLE evolve' #-}
evolve' :: DiscretelyInterpolable v
        => Evolution v
        -> Frame
        -- ^ current frame
        -> w
        -- ^ the value
evolve' (Evolution s _ _ _) (Frame step) =
  interpolateSuccessive' s $ assert (step >= 0) step


{-# INLINABLE evolveIO #-}
evolveIO :: (DiscretelyInterpolable v, Draw e)
         => Evolution v
         -> Frame
         -- ^ current frame
         -> ReaderT e IO ()
evolveIO (Evolution s _ _ _) (Frame step) =
  interpolateSuccessiveIO s $ assert (step >= 0) step
