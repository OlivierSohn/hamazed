{-# LANGUAGE NoImplicitPrelude #-}

-- | This module is about chaining animations.

module Animation.Design.Chain
    (
      chainOnCollision
    ) where


import           Imajuscule.Prelude

import           Animation.Types
import           Animation.Design.Apply


-- | Chains two animations on collision. Note that both animations can be active
-- at the same time because the chaining is local, per individual animation point.
chainOnCollision :: (Coords -> Frame -> ([Coords], Maybe Char))
                 -- ^ Animation 1
                 -> (Coords -> Frame -> ([Coords], Maybe Char))
                 -- ^ Animation 2
                 -> Iteration
                 -- ^ Current iteration
                 -> (Coords -> Location)
                 -- ^ Collision function
                 -> AnimatedPoints
                 -> AnimatedPoints
chainOnCollision anim1 anim2 iteration getLocation tree  =
  let (AnimatedPoints a b branches onWall mayChar) = applyAnimation anim1 iteration getLocation tree
      newBranches = Just $ case branches of
        Nothing -> error "applyAnimation was supposed to create a Just ?"
        Just l ->  map (either (Left . applyAnimation anim2 iteration getLocation) Right) l
  in AnimatedPoints a b newBranches onWall mayChar

-- TODO generic chaining of animations
{--
chainAnimationsOnCollision :: [Coords -> Iteration -> [Coords]]
                           -- ^ each animation function should return a constant number of Coords across iterations
                           -> Iteration
                           -> (Coords -> Location)
                           -- ^ collision function
                           -> AnimatedPoints
                           -> AnimatedPoints
chainAnimationsOnCollision animations iteration getLocation tree = undefined
--}
