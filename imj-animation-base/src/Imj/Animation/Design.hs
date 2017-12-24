{-# OPTIONS_HADDOCK prune #-}

{- |
Animations are recipes to generate animation points.

Two animations can be composed in the following sense : when the interaction
between an animation point of the first animation
and its environment results in a mutation, the animation point stops being active
for the first animation and gives birth to animation points for the second animation
at that position.
-}

module Imj.Animation.Design
          (
            module Imj.Animation.Design.Types
          , module Imj.Animation.Design.Animator
          -- * Update AnimatedPoints
          {- | Functions updating 'AnimatedPoints' can be passed a single pure animation function,
          (see 'applyAnimation') or two pure animations functions, /composing/
          them in the sense of /animation/ composition (see 'composePureAnimations').
          -}
          , module Imj.Animation.Design.Apply
          , module Imj.Animation.Design.Compose
          -- * Render AnimationStep
          , module Imj.Animation.Design.RenderUpdate
          , module Imj.Animation.Design.Color
          , module Imj.Animation.Design.Timing
          , module Imj.Animation.Design.Util
          -- * Reexports
          , module Imj.Color
          , module Imj.Timing
          , module Imj.Iteration
          , Coords
          ) where

import Imj.Animation.Design.Animator
import Imj.Animation.Design.Apply
import Imj.Animation.Design.Color
import Imj.Animation.Design.Compose
import Imj.Animation.Design.RenderUpdate
import Imj.Animation.Design.Timing
import Imj.Animation.Design.Types
import Imj.Animation.Design.Util
import Imj.Color
import Imj.Geo.Discrete(Coords)
import Imj.Iteration
import Imj.Timing
