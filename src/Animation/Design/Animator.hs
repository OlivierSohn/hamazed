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
import           Color
import           Geo( Coords )
import           Render( RenderState )
import           WorldSize( Location )


mkAnimator :: (t -> Coords -> Frame -> [Coords])
           -> (t
               -> Tree
               -> StepType
               -> Animation
               -> (Coords -> Location)
               -> RenderState
               -> IO (Maybe Animation))
           -> t
           -> Animator
mkAnimator pure_ io_ params = Animator (applyAnimation (pure_ params)) (io_ params) colorFromFrame

renderAndUpdate' :: Animator -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
renderAndUpdate' (Animator pure_ io_ colorFunc) = renderAndUpdate pure_ io_ colorFunc
