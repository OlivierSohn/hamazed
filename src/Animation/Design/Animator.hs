{-# LANGUAGE NoImplicitPrelude #-}

-- | This module was created to break a cycle between Color and Animation.Types

module Animation.Design.Animator
    (
       mkAnimator
     , renderAndUpdate'
    ) where


import           Imajuscule.Prelude

import           Animation.Design.Apply
import           Animation.Design.RenderUpdate
import           Animation.Types

import           Animation.Color

import           Timing


mkAnimator :: (t -> Coords -> Frame -> ([Coords], Maybe Char))
           -> (t
               -> Tree
               -> Maybe KeyTime
               -> Animation
               -> (Coords -> Location)
               -> Coords
               -> (Char -> Coords -> LayeredColor -> IO ())
               -> IO (Maybe Animation))
           -> t
           -> Animator
mkAnimator pure_ io_ params = Animator (applyAnimation (pure_ params)) (io_ params) colorFromFrame

renderAndUpdate' :: Animator
                 -> Tree
                 -> Maybe KeyTime
                 -> Animation
                 -> (Coords -> Location)
                 -> Coords
                 -> (Char -> Coords -> LayeredColor -> IO ())
                 -> IO (Maybe Animation)
renderAndUpdate' (Animator pure_ io_ colorFunc) = renderAndUpdate pure_ io_ colorFunc
