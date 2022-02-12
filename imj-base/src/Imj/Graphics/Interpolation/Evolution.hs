{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Graphics.Interpolation.Evolution
         (
         -- * Evolution
{- | 'Evolution' is a helper type to interpolate between 'DiscreteInterpolation's
or morph between 'DiscreteMorphing'.

It stores the 'distance' to cache potential expensive distance computations.

The preferred way to create it is to use 'mkEvolutionEaseQuart' which uses the
inverse ease function 'invQuartEaseInOut'.

To produce the desired /easing/ effect, the 'Evolution' should be updated
at specific time intervals. In that respect, 'getDeltaTimeToNextFrame'
computes the next time at which the interpolation should be updated (for interpolations)
or rendered (for morphings), based on the current frame and the inverse ease function.
-}
           Evolution(..)
         , mkEmptyEvolution
         , mkEvolutionEaseQuart
         , mkEvolutionEaseInQuart
         , mkEvolution
         , getDeltaTimeToNextFrame
         -- ** Getting an interpolated value
         , getValueAt
         -- ** Draw a morphing
         , drawMorphingAt
         -- ** Synchronizing multiple Evolutions
         -- | 'EaseClock' can be used to synchronize multiple 'Evolution's.
         , EaseClock(..)
         , mkEaseClock
         ) where

import           GHC.Show(showString)

import           Imj.Prelude
import qualified Prelude as Unsafe(last)

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Class.DiscreteMorphing
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Math.Ease
import           Imj.Iteration
import           Imj.Timing


-- TODO we could optimize by precomputing the lastframes of each individual segment,
-- and select the interval without having to recompute every distance.
-- We could change the Successive type to store the cumulated distance,
-- then do a binary search
-- | Defines an evolution (interpolation or morphing) between 'Successive' 'DiscreteDistance's.
data Evolution v = Evolution {
    getSuccessives :: !(Successive v)
  -- ^ 'Successive' 'DiscreteDistance's.
  , getLastFrame :: {-# UNPACK #-} !Frame
  -- ^ The frame at which the 'Evolution' value is equal to the last 'Successive' value.
  , _evolutionDuration :: {-# UNPACK #-} !(Time Duration System)
  -- ^ Duration of the interpolation
  , _evolutionInverseEase :: !(Double -> Double)
  -- ^ Inverse ease function.
} deriving (Generic)
instance Functor Evolution where
  fmap f (Evolution s a b c) = Evolution (fmap f s) a b c
  {-# INLINE fmap #-}
instance (HasReferencePosition a) => HasReferencePosition (Evolution a) where
  getPosition (Evolution x _ _ _) = getPosition x
  {-# INLINE getPosition #-}
instance (GeoTransform a) => GeoTransform (Evolution a) where
  transform f = fmap (transform f)
  {-# INLINE transform #-}
instance (Show v) => Show (Evolution v) where
  showsPrec _ (Evolution a b c _) = showString $ "Evolution{" ++ show a ++ show b ++ show c ++ "}"
instance (PrettyVal v) => PrettyVal (Evolution v) where
  prettyVal (Evolution a b c _) = prettyVal (a,b,c)

mkEmptyEvolution :: Evolution v
mkEmptyEvolution = Evolution (Successive []) 0 zeroDuration id

{-# INLINABLE mkEvolutionEaseQuart #-}
-- | An evolution between n 'DiscreteDistance's. With a 4th order ease in & out.
mkEvolutionEaseQuart :: DiscreteDistance v
                     => Successive v
                     -- ^ 'DiscreteDistance's through which the evolution will pass.
                     -> Time Duration System
                     -- ^ Duration
                     -> Evolution v
mkEvolutionEaseQuart =
  mkEvolution invQuartEaseInOut

{-# INLINABLE mkEvolutionEaseInQuart #-}
-- | An evolution between n 'DiscreteDistance's. With a 4th order ease in.
mkEvolutionEaseInQuart :: DiscreteDistance v
                     => Successive v
                     -- ^ 'DiscreteDistance's through which the evolution will pass.
                     -> Time Duration System
                     -- ^ Duration
                     -> Evolution v
mkEvolutionEaseInQuart =
  mkEvolution $ inOutToIn invQuartEaseInOut


-- | An evolution between n 'DiscreteDistance's. With a user-specified (inverse) ease function.
{-# INLINABLE mkEvolution #-}
mkEvolution :: DiscreteDistance v
            => (Double -> Double)
            -- ^ Inverse continuous ease function
            -> Successive v
            -- ^ 'DiscreteDistance's through which the evolution will pass.
            -> Time Duration System
            -- ^ Duration
            -> Evolution v
mkEvolution ease s duration =
  let nSteps = distanceSuccessive s
      lastFrame = Frame $ pred nSteps
  in Evolution s lastFrame duration (discreteAdaptor ease nSteps)

-- | Used to synchronize multiple 'Evolution's.
newtype EaseClock = EaseClock (Evolution NotWaypoint) deriving (Show, Generic, PrettyVal)

newtype NotWaypoint = NotWaypoint () deriving(Show, Generic)
instance PrettyVal NotWaypoint where
  prettyVal _ = prettyVal "NotWaypoint"
-- | To make sure that we never use distance on an 'EaseClock'.
instance DiscreteDistance NotWaypoint where
  distance = error "don't use distance on NotWaypoint"

-- | Constructor of 'EaseClock'
mkEaseClock :: Time Duration System
            -- ^ Duration
            -> Frame
            -- ^ Last frame
            -> (Double -> Double)
            -- ^ Inverse ease function (value -> time, both between 0 and 1)
            -> EaseClock
mkEaseClock duration lastFrame ease =
  let nSteps = fromIntegral $ succ lastFrame
  in EaseClock $ Evolution (Successive []) lastFrame duration (discreteAdaptor ease nSteps)

-- | Computes the time increment between the input 'Frame' and the next.
getDeltaTimeToNextFrame :: Evolution v
                        -> Frame
                        -> Maybe (Time Duration System)
                        -- ^ If evolution is still ongoing, returns the time interval
                        --      between the input 'Frame' and the next.
getDeltaTimeToNextFrame (Evolution _ lastFrame@(Frame lastStep) duration easeValToTime) frame@(Frame step)
  | frame < 0          = error "negative frame"
  | frame >= lastFrame = Nothing
  | otherwise          = Just $ (easeValToTime targetValue - easeValToTime thisValue) .* duration
  where
    nextStep = succ step
    thisValue = fromIntegral step / fromIntegral lastStep
    targetValue = fromIntegral nextStep / fromIntegral lastStep


{-# INLINABLE getValueAt #-}
-- | Gets the value of an 'Evolution' at a given 'Frame'.
getValueAt :: DiscreteInterpolation v
           => Evolution v
           -> Frame
           -> v
           -- ^ The evolution value.
getValueAt (Evolution (Successive []) _ _ _) _ = error "logic"
getValueAt (Evolution s@(Successive l@(v:_)) lastFrame _ _) frame@(Frame step)
  | frame <= 0         = v
  | frame >= lastFrame = Unsafe.last l
  | otherwise          = interpolateSuccessive s step


{-# INLINABLE drawMorphingAt #-}
-- | Draws an 'Evolution' at a given 'Frame'.
drawMorphingAt :: (DiscreteMorphing v, Draw e, MonadReader e m, MonadIO m)
            => Evolution v
            -> Frame
            -> m ()
drawMorphingAt (Evolution s _ _ _) (Frame step) =
  drawMorphingSuccessive s $ assert (step >= 0) step
