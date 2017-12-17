{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
= Preamble

This module defines the types related to animations.

Animations are recipes to generate animation points, and animations can be chained
to produce complex effects. When two animations are chained, one animation point
of the first animation gives birth to animation points for the second animation when it
collides with its environment.

-}

module Animation.Types
    (
    -- * Animated points
      AnimatedPoints(..)
    -- ** How they react to collisions
    , CollisionReaction(..)
    -- ** Constructor
    , mkAnimatedPoints
    -- * Updating the animation
    , AnimationUpdate(..)
    -- ** Handling the first frame
    , AnimationZero(..)
    -- ** Constructor
    , mkAnimationUpdate
    , Animator(..)
    , StepType(..)
    -- Reexports
    , module Iteration
    , Location(..)
    , Coords
    ) where


import           Imajuscule.Prelude

import           GHC.Show(showString)

import           Collision(Location(..))

import           Color.Types

import           Geo.Discrete.Types( Coords )

import           Timing( KeyTime )

import           Iteration


{- |  Tracks the location and state of each animation point in a recursive fashion, allowing
every single animation point to be the starting point of a new animation.
-}
data AnimatedPoints = AnimatedPoints {
    _treeRoot :: !Coords
    -- ^ Where the animation begins
  , _treeStart :: !Frame
    -- ^ When the animation begins (relatively to the parent animation if any)
  , _treeBranches :: !(Maybe [Either AnimatedPoints Coords])
    -- ^ The 'Just' list contains one element per animation point of the animation
    --  corresponding to this recursion depth. Elements are 'Either':
    --
    -- * 'Coords' when they are still alive.
    -- * 'AnimatedPoints' when they are dead for this animation and maybe
    --   gave birth to animation points in the next chained animation.
  , _treeOnWall :: !CollisionReaction
    -- ^ What the animation points do when they meet a wall
  , _treeRenderedWith :: !(Maybe Char)
}

-- | A recursive type encoding how animation points react to collisions.
--
-- The n-th recursion level corresponds to the behaviour for the n-th chained animation.
data CollisionReaction =
        Traverse
        -- ^ Collisions are ignored for this animation.
        -- Hence, the animation terminates only if its pure animation function
        -- returns an empty list for each frame after a given frame.
        | ReboundAnd CollisionReaction
        -- ^ When an animation point collides with its environment, it "evolves"
        -- and gives birth to a new set of animation points using the next animation.
        | Stop
        -- ^ Data structure termination.


-- | Initializes 'AnimatedPoints' with a location defining the start of the first animation.
mkAnimatedPoints :: Coords
                 -- ^ Where the first animation should start.
                 -> CollisionReaction
                 -- ^ How should animated points behave on collisions.
                 -> AnimatedPoints
mkAnimatedPoints c ow =
  AnimatedPoints c 0 Nothing ow Nothing


-- TODO use this generalize animation chaining ?
{--
data Continuation = Continuation {
    _continuationFunction :: !(),
    _continuationOnWall :: !CollisionReaction
}
--}

{- | Contains what is required to do /one/ update of 'AnimatedPoints',
and to generate the next 'AnimationUpdate', or 'Nothing' if the animation is over.
-}
data AnimationUpdate e = AnimationUpdate {
    _animationNextTime :: !KeyTime
    -- ^ The time at which this animation becomes obsolete
  , _animationIteration :: !Iteration
    -- ^ The iteration
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to render the animation points, if the pure animation function doesn't specify one.
  , _animationRender :: !(Maybe KeyTime
                       -> AnimationUpdate e
                       -> (Coords -> Location)
                       -> Coords
                       -> ReaderT e IO (Maybe (AnimationUpdate e))
                       )
    -- ^ Renders 'AnimatedPoints' which are pre-applied, as well as input parameters.
    -- may return the next 'AnimationUpdate'
}

instance Show (AnimationUpdate e) where
  showsPrec _ (AnimationUpdate a b c _) = showString $ "AnimationUpdate{" ++ show (a,b,c) ++ "}"


-- | Specifies if the zero frame should be skipped or not.
data AnimationZero = WithZero
                   | SkipZero


-- Specifies what should be updated.
data StepType = Initialize
              -- ^ Update 'AnimatedPoints'
              | Update
              -- ^ Update 'AnimatedPoints' and 'Iteration'
              | Same
              -- ^ Update 'Iteration'


-- | Constructs an 'AnimationUpdate'
mkAnimationUpdate :: (Maybe KeyTime -> AnimationUpdate e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (AnimationUpdate e)))
                  -- ^ The update function.
                  -> KeyTime
                  -- ^ The 'KeyTime' of the event that triggered this animation.
                  -> AnimationZero
                  -- ^ Should we keep or skip the 0 frame.
                  -> Speed
                  -- ^ The 'Speed' of the animation (the 'Frame' will be incremented at this rate)
                  -> Maybe Char
                  -- ^ The default 'Char' to use when the pure animation function doesn't specify one.
                  -> AnimationUpdate e
mkAnimationUpdate render t frameInit speed mayChar =
  let mayNext = case frameInit of
                  WithZero -> id
                  SkipZero -> nextIteration
  in AnimationUpdate t (mayNext $ zeroIteration speed) mayChar render


-- Intermediate helper structure to construct an 'AnimationUpdate'
data Animator e = Animator {
    _animatorPure :: !(Iteration -> (Coords -> Location) -> AnimatedPoints -> AnimatedPoints)
    -- ^ A pure animation function that updates AnimatedPoints
  , _animatorIO :: AnimatedPoints -> Maybe KeyTime -> AnimationUpdate e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (AnimationUpdate e))
    -- ^ An IO function that consumes an updated AnimatedPoints to render the animation.
    --
    -- Non-strict to avoid an infinite loop (cf https://ghc.haskell.org/trac/ghc/ticket/14521)
  , _animatorColorFromFrame :: !(Frame -> LayeredColor)
    -- ^ A color function
}
