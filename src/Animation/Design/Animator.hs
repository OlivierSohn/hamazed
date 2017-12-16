{-# LANGUAGE NoImplicitPrelude #-}

-- | This module exposes Animator-related functions.

module Animation.Design.Animator
    (
       mkAnimator
     , renderAndUpdate'
    ) where


import           Imajuscule.Prelude

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
               -> AnimationUpdate e
               -> (Coords -> Location)
               -> Coords
               -> ReaderT e IO (Maybe (AnimationUpdate e)))
           -> t
           -> Animator e
mkAnimator pure_ io_ params =
  Animator (applyAnimation (pure_ params)) (io_ params) colorFromFrame


{-# INLINABLE renderAndUpdate' #-}
renderAndUpdate' :: (Draw e)
                 => Animator e
                 -> AnimatedPoints
                 -> Maybe KeyTime
                 -> AnimationUpdate e
                 -> (Coords -> Location)
                 -> Coords
                 -> ReaderT e IO (Maybe (AnimationUpdate e))
renderAndUpdate' (Animator pure_ io_ colorFunc) =
  renderAndUpdate pure_ io_ colorFunc
