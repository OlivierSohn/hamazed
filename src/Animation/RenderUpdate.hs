{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Animation.RenderUpdate
    (
        renderAndUpdateAnimations
    ) where

import           Imajuscule.Prelude

import           Data.Maybe( catMaybes )

import           Animation.Types
import           Geo( Coords )
import           Render( RenderState )
import           Timing( KeyTime
                       , addAnimationStepDuration )
import           WorldSize( Location )



computeStep :: Maybe KeyTime -> Animation -> StepType
computeStep mayKey (Animation k' (Iteration(_,frame)) _)
  | frame == zeroFrame = Update -- initialize step
  | otherwise          = maybe Same (\k -> if k == k' then Update else Same) mayKey


update :: StepType
       -> Animation
       -> Animation
update = \case
            Update -> stepAnimation
            Same   -> id

stepAnimation :: Animation
              -> Animation
stepAnimation (Animation t i f) = Animation (addAnimationStepDuration t) (nextIteration i) f


renderAndUpdateAnimations :: Maybe KeyTime
                          -> (Coords -> Location)
                          -> RenderState
                          -> [Animation]
                          -> IO [Animation]
renderAndUpdateAnimations k getLocation r anims =
  catMaybes <$> mapM (\a@(Animation _ _ render) -> do
    let step = computeStep k a
        a' = update step a
    render step a' getLocation r) anims
