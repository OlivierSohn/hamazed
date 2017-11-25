{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Animation.Types
    (
    -- Animator
      Animator(..)
    -- | Animation and constructor
    , Animation(..)
    , mkAnimation
    -- |
    , Tree(..)
    , mkAnimationTree
    -- |
    , StepType(..)
    -- | Iteration and constructors
    , Iteration(..)
    , zeroIteration
    , nextIteration
    , previousIteration
    -- | Speed
    , Speed(..)
    -- | Frame and constructors
    , Frame(..)
    , zeroFrame
    ) where


import           Imajuscule.Prelude

import           GHC.Generics( Generic )

import           System.Console.ANSI(Color8Code)

import           Geo( Coords )
import           Render( RenderState )
import           Timing( KeyTime )
import           WorldSize( Location )

-- | Animator contains functions to update and render an Animation.
data Animator a = Animator {
    _animatorPure :: !(Iteration -> (Coords -> Location) -> Tree -> Tree)
    -- ^ a function that updates Tree
  , _animatorIO   :: !(Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
    -- ^ a function that consumes Tree to render the animation
  , _animatorColorFromFrame :: !(Frame -> Color8Code)
    -- ^ a function that assigns a color to an animation frame
}

-- | Tracks each animation point state in a recursive fashion, allowing
--   every single animation point (an animation typically generates multiple points
--   at the same time) to be the starting point of a new animation (when it touches
--   a wall for example).
data Tree = Tree {
    _treeRoot :: !Coords
    -- ^ where the animation begins
  , _treeStart :: !Frame
    -- ^ when the animation begins (relatively to the parent animation if any)
  , _treeBranches :: !(Maybe [Either Tree Coords])
    -- ^ There is one element in the list per animation point.
    -- 'Right Coords' elements are still alive (typically they didn't collide yet with the world).
    -- 'Left Tree' elements are dead for this animation and maybe gave birth to another animation.
}

data Animation = Animation {
    _animationNextTime :: !KeyTime
    -- ^ The time at which this animation becomes obsolete
  , _animationIteration :: !Iteration
    -- ^ The iteration
  , _animationRender :: !(StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
    -- ^ This function renders the animation (input parameters and state (Tree) are pre-applied)
    --   and may return an updated Animation
}

data StepType = Update
              | Same

newtype Iteration = Iteration (Speed, Frame) deriving(Generic, Eq, Show)
newtype Speed = Speed Int deriving(Generic, Eq, Show, Num)
newtype Frame = Frame Int deriving(Generic, Eq, Show, Num)

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

mkAnimationTree :: Coords -> Tree
mkAnimationTree c = Tree c 0 Nothing


mkAnimation :: (StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
            -> KeyTime
            -> Speed
            -> Animation
mkAnimation render t speed = Animation t {-do not increment, it will be done while rendering-} (zeroIteration speed) render


zeroIteration :: Speed -> Iteration
zeroIteration s = Iteration (s,zeroFrame)

nextIteration :: Iteration -> Iteration
nextIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i + speed))

previousIteration :: Iteration -> Iteration
previousIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i - speed))


zeroFrame :: Frame
zeroFrame = Frame 0
