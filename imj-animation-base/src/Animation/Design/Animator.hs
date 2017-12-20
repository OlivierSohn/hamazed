{-# LANGUAGE NoImplicitPrelude #-}

-- | This module exposes Animator-related functions.

module Animation.Design.Animator
    (
       mkAnimator
     , renderAndUpdate'
    ) where


import           Imajuscule.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Animation.Color
import           Animation.Design.Apply
import           Animation.Design.RenderUpdate
import           Animation.Types

import           Draw
import           Timing


mkAnimator :: (t -> Coords -> Frame -> ([Coords], Maybe Char))
           -> (t
               -> AnimatedPoints
               -> Maybe KeyTime
               -> AnimationUpdate m
               -> (Coords -> InteractionResult)
               -> Coords
               -> m (Maybe (AnimationUpdate m)))
           -> t
           -> Animator m
mkAnimator pure_ io_ params =
  Animator (applyAnimation (pure_ params)) (io_ params) colorFromFrame


{-# INLINABLE renderAndUpdate' #-}
renderAndUpdate' :: (Draw e, MonadReader e m, MonadIO m)
                 => Animator m
                 -> AnimatedPoints
                 -> Maybe KeyTime
                 -> AnimationUpdate m
                 -> (Coords -> InteractionResult)
                 -> Coords
                 -> m (Maybe (AnimationUpdate m))
renderAndUpdate' (Animator pure_ io_ colorFunc) =
  renderAndUpdate pure_ io_ colorFunc
