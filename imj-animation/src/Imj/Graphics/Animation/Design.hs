{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Animation.Design
          (  -- * Animation framework
            Animation
            {- | An 'Animation' is updated every 'animationPeriod' and generates
            'AnimatedPoint's: -}
          , AnimatedPoint(..)
            {- | 'AnimatedPoint's live in a
            <https://en.wikipedia.org/wiki/Tree_(graph_theory) tree-like structure>:
            -}
          , AnimatedPoints(..)
          , CanInteract(..)
          , InteractionResult(..)
            -- ** Create an Animation
          {-| Upon creation using 'mkAnimation', an 'Animation' contains a /root/
          'AnimatedPoints' containing no 'AnimatedPoint': -}
          , mkAnimation
            -- ** Update an Animation
          {-| The update of an 'Animation' creates and mutates 'AnimatedPoint's: -}
          , updateAnimationIfNeeded
            -- ** Render an Animation
          , renderAnim
            -- * Utilities
          , earliestDeadline
          , module Imj.Graphics.Animation.Design.Timing
          , module Imj.Graphics.Animation.Design.Color
          -- * Reexports
          , module Imj.Timing
          , module Imj.Iteration
          , Coords
          ) where

import Imj.Geo.Discrete(Coords)
import Imj.Graphics.Animation.Design.Types
import Imj.Graphics.Animation.Design.Color
import Imj.Graphics.Animation.Design.Create
import Imj.Graphics.Animation.Design.Render
import Imj.Graphics.Animation.Design.Timing
import Imj.Graphics.Animation.Design.Update
import Imj.Iteration
import Imj.Timing
