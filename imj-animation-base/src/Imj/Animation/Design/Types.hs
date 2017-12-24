{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Types
    (
    -- * Types
    -- ** Animated points
      AnimatedPoints(..)
    -- *** Environment interactions
    , CanInteract(..)
    , InteractionResult(..)
    -- *** Constructor
    , mkAnimatedPoints
    -- ** Updating the animation
    , AnimationStep(..)
    -- *** Handling the first frame
    , AnimationZero(..)
    -- *** Constructor
    , mkAnimationUpdate
    , Animator(..)
    , StepType(..)
    ) where


import           Imj.Prelude

import           GHC.Show(showString)

import           Imj.Color.Types
import           Imj.Geo.Discrete.Types(Coords)
import           Imj.Iteration
import           Imj.Timing(KeyTime)


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
        --
        -- Hence, the animation terminates only if its pure animation function
        -- returns an empty list for each frame after a given frame.
        | Interact CanInteract
        -- ^ Animation points can be mutated after an interaction with the environment.
        | Stop
        -- ^ Data structure termination.

-- | The result of an interaction between an animation point and the environment.
data InteractionResult = Mutation
                       -- ^ The animation point will mutate, if it is allowed
                       -- to interact with the environment.
                       | Stable
                       -- ^ The animation point will not mutate.
                       deriving(Eq,Show)

-- | Constructs an 'AnimatedPoints'.
mkAnimatedPoints :: Coords
                 -- ^ Where the first animation should start.
                 -> CanInteract
                 -- ^ Are animated points allowed to interact with the environment?
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
and to generate the next 'AnimationStep', or 'Nothing' if the animation is over.
-}
data AnimationStep m = AnimationStep {
    _animationNextTime :: !KeyTime
    -- ^ The time at which this step becomes obsolete and should be updated
  , _animationIteration :: !Iteration
    -- ^ The iteration of this step
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to render the animation points, if the pure animation function doesn't specify one.
  , _animationRender :: !(Maybe KeyTime
                       -> AnimationStep m
                       -> (Coords -> InteractionResult)
                       -> Coords
                       -> m (Maybe (AnimationStep m)))
    -- ^ This is the function to call to render 'AnimatedPoints' for this step
    -- (which are pre-applied, as well as input parameters). The function returns
    -- the next 'AnimationStep' (updated or not, depending on 'KeyTime').
    --
    -- Note that the 'AnimationStep' passed to this function is the one that
    -- contains it.
}

instance Show (AnimationStep m) where
  showsPrec _ (AnimationStep a b c _) = showString $ "AnimationStep{" ++ show (a,b,c) ++ "}"


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


-- | Constructs an 'AnimationStep'
mkAnimationUpdate :: (Maybe KeyTime -> AnimationStep m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationStep m)))
                  -- ^ The update function.
                  -> KeyTime
                  -- ^ The 'KeyTime' of the event that triggered this animation.
                  -> AnimationZero
                  -- ^ Should we keep or skip the 0 frame.
                  -> Speed
                  -- ^ The 'Speed' of the animation (the 'Frame' will be incremented at this rate)
                  -> Maybe Char
                  -- ^ The default 'Char' to use when the pure animation function doesn't specify one.
                  -> AnimationStep m
mkAnimationUpdate render t frameInit speed mayChar =
  let mayNext = case frameInit of
                  WithZero -> id
                  SkipZero -> nextIteration
  in AnimationStep t (mayNext $ zeroIteration speed) mayChar render


-- | Intermediate helper structure to construct an 'AnimationStep'
data Animator m = Animator {
    _animatorPure :: !(Iteration -> (Coords -> InteractionResult) -> AnimatedPoints -> AnimatedPoints)
    -- ^ A pure animation function that updates AnimatedPoints
  , _animatorIO :: AnimatedPoints -> Maybe KeyTime -> AnimationStep m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationStep m))
    -- ^ An IO function that consumes an updated AnimatedPoints to render the animation.
    --
    -- Non-strict to avoid an infinite loop (cf https://ghc.haskell.org/trac/ghc/ticket/14521)
  , _animatorColorFromFrame :: !(Frame -> LayeredColor)
    -- ^ A color function
}
