{-# OPTIONS_HADDOCK hide #-}

module Imj.Animation.Design.Internal.Types
          ( AnimatedPoints(..)
          , CanInteract(..)
          , InteractionResult(..)
          ) where

import           Imj.Geo.Discrete
import           Imj.Iteration

{- |  Contains the position of each animated point.
-}
data AnimatedPoints = AnimatedPoints {
    _animatedPointsTreeRoot :: !Coords
    -- ^ Where the animation begins
  , _animatedPointsTreeStart :: !Frame
    -- ^ When the animation begins (relatively to the parent animation if any)
  , _animatedPointsTreeBranches :: !(Maybe [Either AnimatedPoints Coords])
    -- ^ The 'Just' list elements are:
    --
    -- * 'Coords' as long as the animated point is 'Stable'.
    -- * 'AnimatedPoints' after a 'Mutation'.
  , _animatedPointsMayInteract :: !CanInteract
    -- ^ Can the animated points interact with their environment?
  , _animatedPointsRenderedWith :: !(Maybe Char)
    -- ^ The char that should be used to render the points.
} deriving (Show)


-- | A recursive type indicating if animated points can interact with the environment.
--
-- The @n@-th recursion level applies to animated points of
-- depth @n@ in 'AnimatedPoints'.
data CanInteract =
          DontInteract
        -- ^ Interaction results are ignored for this animation.
        --
        -- Hence, the animation terminates only if its geometric animation function
        -- returns an empty list for each frame after a given frame.
        | Interact CanInteract
        -- ^ Animated points can be mutated after an interaction with the environment.
        | Stop
        -- ^ Data structure termination.
        deriving (Show)


-- | The result of an interaction between an animated point and the environment.
data InteractionResult = Mutation
                       -- ^ The animated point will mutate, if it is allowed
                       -- to interact with the environment.
                       | Stable
                       -- ^ The animated point will not mutate.
                       deriving(Eq,Show)


-- TODO use this generalize animation chaining ?
{--
data Continuation = Continuation {
    _continuationFunction :: !(),
    _continuationOnWall :: !CanInteract
}
--}
