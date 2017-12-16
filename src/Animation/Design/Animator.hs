{-# LANGUAGE NoImplicitPrelude #-}

-- | This module was created to break a cycle between Color and Animation.Types

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

import           Timing


mkAnimator :: (t -> Coords -> Frame -> ([Coords], Maybe Char))
           -> (t
               -> Tree
               -> Maybe KeyTime
               -> Animation e
               -> (Coords -> Location)
               -> Coords
               -> ReaderT e IO (Maybe (Animation e)))
           -> t
           -> Animator e
mkAnimator pure_ io_ params = Animator (applyAnimation (pure_ params)) (io_ params) colorFromFrame


{-# INLINABLE renderAndUpdate' #-}
renderAndUpdate' :: (Draw e)
                 => Animator e
                 -> Tree
                 -> Maybe KeyTime
                 -> Animation e
                 -> (Coords -> Location)
                 -> Coords
                 -> ReaderT e IO (Maybe (Animation e))
renderAndUpdate' (Animator pure_ io_ colorFunc) = renderAndUpdate pure_ io_ colorFunc
