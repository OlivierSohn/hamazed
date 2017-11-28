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


-- | this function doesn't handle the Initialize step,
--   because the Tree is needed to decide if it's initialize.
-- TODO refactor and compute the step inside render where we have all the info needed. (needs https://ghc.haskell.org/trac/ghc/ticket/14521 to be fixed first)
computeStep :: Maybe KeyTime -> Animation -> StepType
computeStep mayKey (Animation k' _ _ _) =
  maybe
    Same
    (\k -> if k == k' then Update else Same)
      mayKey


update :: StepType
       -> Animation
       -> Animation
update = \case
            Update -> stepAnimation
            _      -> id

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
  a' <- render step a getLocation r -- TODO pass keyTime instead of step, do the update inside
  return $ fmap (update step) a'
