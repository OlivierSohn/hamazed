{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
= Preamble

This module defines the types related to animations.

Animations are recipes to generate animation points, and two animations can be
chained (composed) to produce complex effects. In that case, when an animation point of the
first animation first collides with its environment, it stops being active for
the first animation and gives birth to animation points for the second animation
at that position.

-}

module Imj.Animation.Types
    (
    -- * Animated points
      AnimatedPoints(..)
    -- ** How they interact with their environment
    , CanInteract(..)
    , InteractionResult(..)
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
    , module Imj.Iteration
    , Coords
    ) where


import           Imj.Prelude

import           GHC.Show(showString)

import           Imj.Color.Types

import           Imj.Geo.Discrete.Types(Coords)

import           Imj.Timing(KeyTime)

import           Imj.Iteration


{- |  Tracks the position and state of each animation point in a recursive fashion, allowing
every single animation point to be the starting point of a new animation.
-}
data AnimatedPoints = AnimatedPoints {
    _animatedPointsTreeRoot :: !Coords
    -- ^ Where the animation begins
  , _animatedPointsTreeStart :: !Frame
    -- ^ When the animation begins (relatively to the parent animation if any)
  , _animatedPointsTreeBranches :: !(Maybe [Either AnimatedPoints Coords])
    -- ^ The 'Just' list contains one element per animation point of the animation
    --  corresponding to this recursion depth. Elements are 'Either':
    --
    -- * 'Coords' when they are still stable.
    -- * 'AnimatedPoints' when they mutated and maybe
    --   gave birth to animation points in the next chained animation.
  , _animatedPointsMayInteract :: !CanInteract
    -- ^ Can the animation points interact with their environment?
  , _animatedPointsRenderedWith :: !(Maybe Char)
}

-- | Can animation points interact with the environment?
--
-- The n-th recursion level defines the behaviour for the n-th animation.
data CanInteract =
          DontInteract
        -- ^ Interaction results are ignored for this animation.
        -- Hence, the animation terminates only if its pure animation function
        -- returns an empty list for each frame after a given frame.
        | Interact CanInteract
        -- ^ Animation points can be mutated after an interaction with the environment,
        -- thus giving birth to a new set of animation points in the next animation.
        | Stop
        -- ^ Data structure termination.

data InteractionResult = Mutation
                       | Stable
                       deriving(Eq,Show)

-- | Initializes 'AnimatedPoints' with a InteractionResult defining the start of the first animation.
mkAnimatedPoints :: Coords
                 -- ^ Where the first animation should start.
                 -> CanInteract
                 -- ^ How should animated points behave on collisions.
                 -> AnimatedPoints
mkAnimatedPoints c ow =
  AnimatedPoints c 0 Nothing ow Nothing


-- TODO use this generalize animation chaining ?
{--
data Continuation = Continuation {
    _continuationFunction :: !(),
    _continuationOnWall :: !CanInteract
}
--}

{- | Contains what is required to do /one/ update of 'AnimatedPoints',
and to generate the next 'AnimationUpdate', or 'Nothing' if the animation is over.
-}
data AnimationUpdate m = AnimationUpdate {
    _animationNextTime :: !KeyTime
    -- ^ The time at which this animation becomes obsolete
  , _animationIteration :: !Iteration
    -- ^ The iteration
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to render the animation points, if the pure animation function doesn't specify one.
  , _animationRender :: !(Maybe KeyTime
                       -> AnimationUpdate m
                       -> (Coords -> InteractionResult)
                       -> Coords
                       -> m (Maybe (AnimationUpdate m))
                       )
    -- ^ Renders 'AnimatedPoints' which are pre-applied, as well as input parameters.
    -- may return the next 'AnimationUpdate'
}

instance Show (AnimationUpdate m) where
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
mkAnimationUpdate :: (Maybe KeyTime -> AnimationUpdate m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationUpdate m)))
                  -- ^ The update function.
                  -> KeyTime
                  -- ^ The 'KeyTime' of the event that triggered this animation.
                  -> AnimationZero
                  -- ^ Should we keep or skip the 0 frame.
                  -> Speed
                  -- ^ The 'Speed' of the animation (the 'Frame' will be incremented at this rate)
                  -> Maybe Char
                  -- ^ The default 'Char' to use when the pure animation function doesn't specify one.
                  -> AnimationUpdate m
mkAnimationUpdate render t frameInit speed mayChar =
  let mayNext = case frameInit of
                  WithZero -> id
                  SkipZero -> nextIteration
  in AnimationUpdate t (mayNext $ zeroIteration speed) mayChar render


-- Intermediate helper structure to construct an 'AnimationUpdate'
data Animator m = Animator {
    _animatorPure :: !(Iteration -> (Coords -> InteractionResult) -> AnimatedPoints -> AnimatedPoints)
    -- ^ A pure animation function that updates AnimatedPoints
  , _animatorIO :: AnimatedPoints -> Maybe KeyTime -> AnimationUpdate m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationUpdate m))
    -- ^ An IO function that consumes an updated AnimatedPoints to render the animation.
    --
    -- Non-strict to avoid an infinite loop (cf https://ghc.haskell.org/trac/ghc/ticket/14521)
  , _animatorColorFromFrame :: !(Frame -> LayeredColor)
    -- ^ A color function
}
