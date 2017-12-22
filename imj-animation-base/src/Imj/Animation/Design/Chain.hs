{-# LANGUAGE NoImplicitPrelude #-}

-- | This module is about chaining animations.

module Imj.Animation.Design.Chain
    (
      chainOnCollision
    ) where


import           Imj.Prelude

import           Imj.Animation.Types
import           Imj.Animation.Design.Apply


-- | Chains two animations on collision. Note that both animations can be active
-- at the same time because the chaining is local, per individual animation point.
chainOnCollision :: (Coords -> Frame -> ([Coords], Maybe Char))
                 -- ^ Animation 1
                 -> (Coords -> Frame -> ([Coords], Maybe Char))
                 -- ^ Animation 2
                 -> Iteration
                 -- ^ Current iteration
                 -> (Coords -> InteractionResult)
                 -- ^ Interaction function
                 -> AnimatedPoints
                 -> AnimatedPoints
chainOnCollision anim1 anim2 iteration interaction tree  =
  let (AnimatedPoints a b branches onWall mayChar) = applyAnimation anim1 iteration interaction tree
      newBranches = Just $ case branches of
        Nothing -> error "applyAnimation should create a Just"
        Just l ->  map (either (Left . applyAnimation anim2 iteration interaction) Right) l
  in AnimatedPoints a b newBranches onWall mayChar

-- TODO generic chaining of animations
{--
chainAnimationsOnCollision :: [Coords -> Iteration -> [Coords]]
                           -- ^ each animation function should return a constant number of Coords across iterations
                           -> Iteration
                           -> (Coords -> InteractionResult)
                           -- ^ collision function
                           -> AnimatedPoints
                           -> AnimatedPoints
chainAnimationsOnCollision animations iteration interaction tree = undefined
--}
