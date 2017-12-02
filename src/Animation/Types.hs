{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Types
    (
    -- Animator
      Animator(..)
    -- | Animation and constructor
    , Animation(..)
    , AnimationZero(..)
    , mkAnimation
    -- |
    , Tree(..)
    , mkAnimationTree
    , OnWall(..)
    -- |
    , StepType(..)
    -- | Speed
    , Speed(..)
    -- | Frame and constructors
    -- | Reexports
    , Coords
    , Location(..)
    , module Iteration
    ) where


import           Imajuscule.Prelude

import           System.Console.ANSI(Color8Code)

import           Collision(Location(..))

import           Geo.Discrete.Types( Coords )

import           Render( RenderState )

import           Timing( KeyTime )

import           Iteration

-- | Animator contains functions to update and render an Animation.
data Animator = Animator {
    _animatorPure :: !(Iteration -> (Coords -> Location) -> Tree -> Tree)
    -- ^ a function that updates Tree
  , _animatorIO :: Tree -> Maybe KeyTime -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
    -- ^ a function that consumes Tree to render the animation.
    -- It is a non-strict field to avoid infinite loop (cf https://ghc.haskell.org/trac/ghc/ticket/14521)
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
  , _treeOnWall :: !OnWall
    -- ^ What the animation points do when they meet a wall
  , _treeRenderedWith :: !(Maybe Char)
}

data OnWall = Traverse -- Collisions are ignored.
                       -- You must ensure that the corresponding pure animation function
                       -- will return a list of 0 coordinates for each frame after a given frame,
                       -- else the animation will never terminate.
            | ReboundAnd OnWall -- On collision, the next sequence of the animation starts.
            | Stop     -- Termination

-- TODO use this generalize animation chaining ?
{--
data Continuation = Continuation {
    _continuationFunction :: !(),
    _continuationOnWall :: !OnWall
}
--}

data Animation = Animation {
    _animationNextTime :: !KeyTime
    -- ^ The time at which this animation becomes obsolete
  , _animationIteration :: !Iteration
    -- ^ The iteration
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to render the animation points
  , _animationRender :: !(Maybe KeyTime -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
    -- ^ This function renders the animation (input parameters and state (Tree) are pre-applied)
    --   and may return an updated Animation
}

data AnimationZero = WithZero
                   | SkipZero

data StepType = Initialize -- update the tree       , iteration doesn't change
              | Update     -- update the tree       , iteration moves forward
              | Same       -- do not update the tree, iteration doesn't change

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

mkAnimationTree :: Coords -> OnWall -> Tree
mkAnimationTree c ow = Tree c 0 Nothing ow Nothing


mkAnimation :: (Maybe KeyTime -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
            -> KeyTime
            -> AnimationZero
            -> Speed
            -> Maybe Char
            -> Animation
mkAnimation render t frameInit speed mayChar =
  let firstIteration =
        (case frameInit of
          WithZero -> id
          SkipZero -> nextIteration)
          $ zeroIteration speed
  in Animation t firstIteration mayChar render
