{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Compose
    ( updateAnimatedPointsUpToDepth2
    ) where


import           Imj.Prelude

import           Imj.Animation.Design.Apply
import           Imj.Animation.Design.Internal.Types
import           Imj.Geo.Discrete
import           Imj.Iteration


{- | Updates the /depth 1/ and /depth 2/ animated points of an 'AnimatedPoints'
using one geometric animation function per depth. -}
updateAnimatedPointsUpToDepth2 :: (Coords -> Frame -> ([Coords], Maybe Char))
                      -- ^ Geometric animation for level 1
                      -> (Coords -> Frame -> ([Coords], Maybe Char))
                      -- ^ Geometric animation for level 2
                      -> Iteration
                      -- ^ Current iteration
                      -> (Coords -> InteractionResult)
                      -- ^ Interaction function
                      -> AnimatedPoints
                      -> AnimatedPoints
updateAnimatedPointsUpToDepth2 anim1 anim2 iteration interaction tree  =
  let (AnimatedPoints a b branches onWall mayChar) =
        updateAnimatedPointsUpToDepth1 anim1 iteration interaction tree
      newBranches = Just $ case branches of
        Nothing -> error "updateAnimatedPointsUpToDepth1 should create a Just"
        Just l ->  map (either (Left . updateAnimatedPointsUpToDepth1 anim2 iteration interaction) Right) l
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
