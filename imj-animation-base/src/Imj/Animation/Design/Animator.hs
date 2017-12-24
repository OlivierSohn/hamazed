{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Animator
    (
      -- * Animator-related functions
       mkAnimator
     , renderAndUpdateIfNeeded'
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Animation.Design.Apply
import           Imj.Animation.Design.Color
import           Imj.Animation.Design.RenderUpdate
import           Imj.Animation.Design.Types
import           Imj.Draw
import           Imj.Iteration
import           Imj.Timing

-- | Creates an 'Animator'
mkAnimator :: (t -> Coords -> Frame -> ([Coords], Maybe Char))
           -> (t
               -> AnimatedPoints
               -> Maybe KeyTime
               -> AnimationStep m
               -> (Coords -> InteractionResult)
               -> Coords
               -> m (Maybe (AnimationStep m)))
           -> t
           -> Animator m
mkAnimator pure_ io_ params =
  Animator (applyAnimation (pure_ params)) (io_ params) colorFromFrame

-- | Creates an 'AnimationStep' from an 'Animator'
{-# INLINABLE renderAndUpdateIfNeeded' #-}
renderAndUpdateIfNeeded' :: (Draw e, MonadReader e m, MonadIO m)
                 => Animator m
                 -> AnimatedPoints
                 -> Maybe KeyTime
                 -> AnimationStep m
                 -> (Coords -> InteractionResult)
                 -> Coords
                 -> m (Maybe (AnimationStep m))
renderAndUpdateIfNeeded' (Animator pure_ io_ colorFunc) =
  renderAndUpdateIfNeeded pure_ io_ colorFunc
