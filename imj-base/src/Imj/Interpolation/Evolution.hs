{-# LANGUAGE NoImplicitPrelude #-}


module Imj.Interpolation.Evolution
         (
         -- * Evolution
{- | 'Evolution' is a helper type to interpolate between 'DiscretelyInterpolable's.
It stores the total interpolation distance and the the inverse ease function.

The preferred way to create it is to use 'mkEvolution'.

To produce the desired /easing/ effect, the interpolation frame should be
incremented at specific time intervals. In that respect, 'getDeltaTimeToNextFrame'
computes the next time at which the interpolation should be updated and rendered,
based on the current frame and the inverse ease function.
-}
           Evolution(..)
         , mkEvolution
         , getDeltaTimeToNextFrame
         -- * Getting - or drawing - the interpolated value
         {- |
 * If the underlying 'DiscretelyInterpolable' instance implements 'interpolate', it
 is valid to call 'getValueAt' but 'drawValueAt' will error.
 * If the underlying 'DiscretelyInterpolable' instance implements 'interpolateIO', it
 is valid to call 'drawValueAt' but 'getValueAt' will error.
         -}
         , getValueAt
         , drawValueAt
         -- * Synchronizing multiple Evolutions
         {- | 'EaseClock' can be used to synchronize
         multiple 'Evolution's.
         -}
         , EaseClock(..)
         , mkEaseClock
         ) where

import           GHC.Show(showString)

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Draw.Class
import           Imj.Interpolation.Class
import           Imj.Iteration
import           Imj.Math.Ease

{-# INLINABLE mkEvolution #-}
-- | An evolution between n values.
mkEvolution :: DiscretelyInterpolable v
            => Successive v
            -- ^ Values through which the evolution will pass.
            -> Float
            -- ^ Duration in seconds
            -> Evolution v
mkEvolution s duration =
  let nSteps = distanceSuccessive s
      lastFrame = Frame $ pred nSteps
  in Evolution s lastFrame duration (discreteInvQuartEaseInOut nSteps)

-- | Used to synchronize multiple 'Evolution's.
newtype EaseClock = EaseClock (Evolution NotDiscretelyInterpolable) deriving (Show)
newtype NotDiscretelyInterpolable = NotDiscretelyInterpolable () deriving(Show)

-- | To make sure that we never use the values of an 'EaseClock'.
instance DiscretelyInterpolable NotDiscretelyInterpolable where
  distance = error "don't use distance on NotDiscretelyInterpolable"
  interpolate = error "don't use interpolate on NotDiscretelyInterpolable"

-- | Constructor of 'EaseClock'
mkEaseClock :: Float
            -- ^ Duration in seconds
            -> Frame
            -- ^ Last frame
            -> (Float -> Float)
            -- ^ Inverse ease function (value -> time, both between 0 and 1)
            -> EaseClock
mkEaseClock duration lastFrame ease =
  let nSteps = fromIntegral $ succ lastFrame
  in EaseClock $ Evolution (Successive []) lastFrame duration (discreteAdaptor ease nSteps)

-- TODO we could optimize by precomputing the lastframes of each individual segment,
-- and select the interval without having to recompute every distance.
-- We could change the Successive type to store the cumulated distance,
-- then do a binary search
-- | Defines an interpolation between 'Successive' 'DiscretelyInterpolable's.
data Evolution v = Evolution {
    _evolutionSuccessive :: !(Successive v)
  -- ^ 'Successive' values.
  , _evolutionLastFrame :: !Frame
  -- ^ The frame at which the 'Evolution' value is equal to the last 'Successive' value.
  , _evolutionDuration :: Float
  -- ^ Duration of the interpolation in seconds.
  , _evolutionInverseEase :: Float -> Float
  -- ^ Inverse ease function.
}

instance (Show v) => Show (Evolution v) where
        showsPrec _ (Evolution a b c _) = showString $ "Evolution{" ++ show a ++ show b ++ show c ++ "}"

-- | Computes the time increment between the input 'Frame' and the next.
getDeltaTimeToNextFrame :: Evolution v
                        -> Frame
                        -> Maybe Float
                        -- ^ If evolution is still ongoing, returns the time interval
                        --      between the input 'Frame' and the next.
getDeltaTimeToNextFrame (Evolution _ lastFrame@(Frame lastStep) duration easeValToTime) frame@(Frame step)
  | frame < 0          = error "negative frame"
  | frame >= lastFrame = Nothing
  | otherwise          = Just dt
  where
    nextStep = succ step
    thisValue = fromIntegral step / fromIntegral lastStep
    targetValue = fromIntegral nextStep / fromIntegral lastStep
    dt = duration * (easeValToTime targetValue - easeValToTime thisValue)


{-# INLINABLE getValueAt #-}
-- | Gets the value of an 'Evolution' at a given 'Frame'. Errors if the 'DiscretelyInterpolable'
-- doesn't implement the 'interpolate' method.
getValueAt :: DiscretelyInterpolable v
           => Evolution v
           -> Frame
           -> v
           -- ^ The evolution value.
getValueAt (Evolution s@(Successive l) lastFrame _ _) frame@(Frame step)
  | frame <= 0         = head l
  | frame >= lastFrame = last l
  | otherwise          = interpolateSuccessive s step


{-# INLINABLE drawValueAt #-}
-- | Draws an 'Evolution' for a given 'Frame'. Errors if the 'DiscretelyInterpolable'
-- doesn't implement the 'interpolateIO' method.
drawValueAt :: (DiscretelyInterpolable v, Draw e, MonadReader e m, MonadIO m)
            => Evolution v
            -> Frame
            -> m ()
drawValueAt (Evolution s _ _ _) (Frame step) =
  interpolateSuccessiveIO s $ assert (step >= 0) step
