{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Compose
    ( composePureAnimations
    ) where


import           Imj.Prelude

import           Imj.Animation.Design.Apply
import           Imj.Animation.Design.Types
import           Imj.Geo.Discrete
import           Imj.Iteration


-- | Composes two pure animation function.
composePureAnimations :: (Coords -> Frame -> ([Coords], Maybe Char))
                      -- ^ Animation 1
                      -> (Coords -> Frame -> ([Coords], Maybe Char))
                      -- ^ Animation 2
                      -> Iteration
                      -- ^ Current iteration
                      -> (Coords -> InteractionResult)
                      -- ^ Interaction function
                      -> AnimatedPoints
                      -> AnimatedPoints
composePureAnimations anim1 anim2 iteration interaction tree  =
  let (AnimatedPoints a b branches onWall mayChar) =
        applyAnimation anim1 iteration interaction tree
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
                           -> AnimatedPoints
                           -> AnimatedPoints
chainAnimationsOnCollision animations iteration interaction tree = undefined
--}
