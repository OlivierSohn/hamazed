{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- | This module exposes Animator-related functions.

module Imj.Animation.Design.Animator
    (
       mkAnimator
     , renderAndUpdate'
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Animation.Color
import           Imj.Animation.Design.Apply
import           Imj.Animation.Design.RenderUpdate
import           Imj.Animation.Types

import           Imj.Draw
import           Imj.Timing


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
