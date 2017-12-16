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
    , Location(..)
    , module Iteration
    , module Draw.Class
    ) where


import           Imajuscule.Prelude

import           GHC.Show(showString)

import           Collision(Location(..))

import           Color.Types

import           Geo.Discrete.Types( Coords )

import           Draw.Class
import           Timing( KeyTime )

import           Iteration

-- | Animator contains functions to update and render an Animation.
data Animator e = Animator {
    _animatorPure :: !(Iteration -> (Coords -> Location) -> Tree -> Tree)
    -- ^ a function that updates Tree
  , _animatorIO :: Tree -> Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e))
    -- ^ a function that consumes Tree to render the animation.
    -- It is a non-strict field to avoid infinite loop (cf https://ghc.haskell.org/trac/ghc/ticket/14521)
  , _animatorColorFromFrame :: !(Frame -> LayeredColor)
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
    -- ^ There is one element in the "Just" list per animation point. Elements are:
    --
    -- * 'Right Coords' when they are still alive (typically they didn't collide yet with the world).
    -- * 'Left Tree' when they are dead for this animation and maybe gave birth to another animation.
  , _treeOnWall :: !OnWall
    -- ^ What the animation points do when they meet a wall
  , _treeRenderedWith :: !(Maybe Char)
}

data OnWall = Traverse -- ^ Collisions are ignored.
                       --
                       -- You must ensure that the corresponding pure animation function
                       -- will return a list of 0 coordinates for each frame after a given frame,
                       -- else the animation will never terminate.
            | ReboundAnd OnWall -- ^ On collision, the next sequence of the animation starts.
            | Stop     -- ^ Termination

-- TODO use this generalize animation chaining ?
{--
data Continuation = Continuation {
    _continuationFunction :: !(),
    _continuationOnWall :: !OnWall
}
--}

data Animation e = Animation {
    _animationNextTime :: !KeyTime
    -- ^ The time at which this animation becomes obsolete
  , _animationIteration :: !Iteration
    -- ^ The iteration
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to render the animation points
  , _animationRender :: !(Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e)))
    -- ^ This function renders the animation (input parameters and state (Tree) are pre-applied)
    --   and may return an updated Animation
}

instance (Draw e) => Show (Animation e) where
        showsPrec _ (Animation a b c _) = showString $ "Animation{" ++ show (a,b,c) ++ "}"

data AnimationZero = WithZero
                   | SkipZero

data StepType = Initialize -- update the tree       , iteration doesn't change
              | Update     -- update the tree       , iteration moves forward
              | Same       -- do not update the tree, iteration doesn't change


mkAnimationTree :: Coords -> OnWall -> Tree
mkAnimationTree c ow = Tree c 0 Nothing ow Nothing

mkAnimation :: (Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e)))
            -> KeyTime
            -> AnimationZero
            -> Speed
            -> Maybe Char
            -> Animation e
mkAnimation render t frameInit speed mayChar =
  let firstIteration =
        (case frameInit of
          WithZero -> id
          SkipZero -> nextIteration)
          $ zeroIteration speed
  in Animation t firstIteration mayChar render
