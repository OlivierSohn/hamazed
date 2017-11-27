{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Animation.RenderUpdate
    (
        renderAndUpdateAnimation
    ) where

import           Imajuscule.Prelude

import           Animation.Types
import           Geo( Coords )
import           Render( RenderState )
import           Timing( KeyTime
                       , addAnimationStepDuration )
import           WorldSize( Location )



computeStep :: Maybe KeyTime -> Animation -> StepType
computeStep mayKey (Animation k' (Iteration(_,frame)) _ _)
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
stepAnimation (Animation t i c f) = Animation (addAnimationStepDuration t) (nextIteration i) c f


renderAndUpdateAnimation :: Maybe KeyTime
                         -> (Coords -> Location)
                         -> RenderState
                         -> Animation
                         -> IO (Maybe Animation)
renderAndUpdateAnimation k getLocation r a@(Animation _ _ _ render) = do
  let step = computeStep k a
      a' = update step a
  render step a' getLocation r
