{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Design.Types
          ( Animation(..)
          , StepType(..)
          , UpdateSpec(..)
          , AnimatedPoints(..)
          , AnimatedPoint(..)
          , CanInteract(..)
          , InteractionResult(..)
          ) where

import           Imj.Prelude

import           GHC.Show(showString)

import           Imj.Geo.Discrete
import           Imj.Iteration
import           Imj.Timing


data Animation = Animation {
    _animationPoints :: !AnimatedPoints
    -- ^ The current points.
  , _animationUpdate :: !((Coords Pos -> InteractionResult)
                      -> Frame
                      -> AnimatedPoints
                      -> AnimatedPoints)
    -- ^ The function updating 'AnimatedPoints'.
  , _animationInteraction :: !(Coords Pos -> InteractionResult)
    -- ^ The environment interaction function.
  , _animationNextUpdateSpec :: !UpdateSpec
    -- ^ The time and iteration of the next update
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to draw animated points when the 'AnimatedPoints'
    -- don't specify one. If 'Nothing', the animation function /must/ specify one
    -- when creating new 'AnimatedPoint's.
}

instance Show Animation where
  showsPrec _ (Animation a _ _ b c) =
    showString $ "Animation{" ++ show (a,b,c) ++ "}"


data UpdateSpec = UpdateSpec {
    _updateSpecTime :: !KeyTime
    -- ^ The time at which the update should happen.
  , _updateSpecIteration :: !Iteration
    -- ^ The iteration that will be used in the update.
} deriving(Show)

-- | Specifies what should be updated in the 'Animation'.
data StepType = Initialize
              -- ^ Update 'AnimatedPoints'
              | Update
              -- ^ Update 'AnimatedPoints' and 'Iteration'
              | Same
              -- ^ Update 'Iteration'


data AnimatedPoints = AnimatedPoints {
    _animatedPointsBranches :: !(Maybe [Either AnimatedPoints AnimatedPoint])
    -- ^ When a 'Right' 'AnimatedPoint' mutates, it is converted to an empty 'Left' 'AnimatedPoints'
  , _animatedPointsCenter :: !(Coords Pos)
    -- ^ The center, aka the coordinates of the 'AnimatedPoint', w.r.t the animation reference frame,
    -- that gave birth to this 'AnimatedPoints'.
  , _animatedPointsFrame :: !Frame
    -- ^ The frame at which this 'AnimatedPoints' was created, relatively to the parent, if any.
} deriving (Show)

data AnimatedPoint = AnimatedPoint {
    _animatedPointCanInteract :: !CanInteract
    -- ^ Can the point interact with the environment?
  , _animatedPointCoords :: {-# UNPACK #-} !(Coords Pos)
    -- ^ Its location, w.r.t the animation reference frame.
  , _animatedPointRenderedWith :: !(Maybe Char)
    -- ^ The char used to render it. If 'Nothing', 'Animation' /must/ specify a 'Char'.
} deriving (Show)

data CanInteract = DontInteract
                 -- ^ The 'AnimatedPoint' can't interact with the environment.
                 --
                 -- To ensure that the 'Animation' is finite in time,
                 -- animation functions returning 'AnimatedPoint's that
                 -- 'DontInteract' should return an empty list of 'AnimatedPoint's
                 -- for each 'Frame' after a given 'Frame'.
                 | Interact
                 -- ^ The 'AnimatedPoint' can be mutated after an interaction
                 -- with the environment.
                 --
                 -- For the animation to be finite in time, 'AnimatedPoint's that
                 -- 'Interact' /must/ eventually be mutated by the environment.
                 --
                 -- Hence, assuming the environment has a finite size,
                 -- animation functions returning 'AnimatedPoint's that
                 -- 'Interact' can guarantee animation finitude by computing their
                 -- coordinates using functions diverging in the 'Frame' argument.
                 deriving (Show, Eq)

data InteractionResult = Mutation
                       -- ^ The 'AnimatedPoint' can mutate.
                       | Stable
                       -- ^ The 'AnimatedPoint' doesn't mutate.
                       deriving(Eq,Show)
