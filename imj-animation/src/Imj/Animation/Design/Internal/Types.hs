{-# OPTIONS_HADDOCK hide #-}

module Imj.Animation.Design.Internal.Types
          ( AnimatedPoints(..)
          , AnimatedPoint(..)
          , CanInteract(..)
          , InteractionResult(..)
          ) where

import           Imj.Geo.Discrete
import           Imj.Iteration

{- | 'AnimatedPoints' can be seen as a
<https://en.wikipedia.org/wiki/Tree_(graph_theory) tree>, whith

* internal nodes that are 'AnimatedPoints' with 'Just' branches.
* leaves that are one of:

    * 'AnimatedPoint'
    * 'AnimatedPoints' with 'Nothing' branches

* a <https://en.wikipedia.org/wiki/Branching_factor branching factor> @=@
the number of 'AnimatedPoint' returned by a call to the /preapplied '***Geo' functions/
that generate the first-level 'AnimatedPoint's for this 'AnimatedPoints'.

[Growth]
'AnimatedPoints' is said to /grow/ when an interaction between one of its 'AnimatedPoint's
and the environment results in a 'Mutation' into a new 'AnimatedPoints' with 'Nothing' branches.

[Center]
The /center/ of an 'AnimatedPoints' is the location from which the
/corresponding visual animation/ will start.
Hence, the center will be passed to the /preapplied '***Geo' functions/ to allow
them to place 'AnimatedPoint's accordingly.

(TODO find a better name for /corresponding visual animation/ : it is the animation
of first-level only 'AnimatedPoint's of this 'AnimatedPoints')

(TODO find a better name for /preapplied '***Geo' functions/ : it is the functions
that, given a 'Frame' and a center, create 'AnimatedPoint's. Proposal : /animating functions/)
-}
data AnimatedPoints = AnimatedPoints {
    _animatedPointsFrame :: !Frame
    -- ^ The frame at which this 'AnimatedPoints' was initialized, relatively to the parent, if any.
  , _animatedPointsCenter :: !Coords
    -- ^ The center, w.r.t the animation reference frame.
  , _animatedPointsBranches :: !(Maybe [Either AnimatedPoints AnimatedPoint])
} deriving (Show)

-- | Represents an animated point.
data AnimatedPoint = AnimatedPoint {
    _animatedPointCanInteract :: !CanInteract
    -- ^ Can the point interact with the environment?
  , _animatedPointCoords :: {-# UNPACK #-} !Coords
    -- ^ Its location, w.r.t the animation reference frame.
  , _animatedPointRenderedWith :: !(Maybe Char)
    -- ^ The char used to render it. If 'Nothing', 'Animation' /must/ specify a 'Char'.
} deriving (Show)

-- | A type indicating if an 'AnimatedPoint' can interact with the environment.
data CanInteract = DontInteract
                 -- ^ No interaction is allowed.
                 --
                 -- To ensure that the animation is finite in time,
                 -- geometric animation functions returning 'AnimatedPoint's that
                 -- 'DontInteract' should return an empty list of 'AnimatedPoint's
                 -- for each 'Frame' after a given 'Frame'.
                 --
                 -- TODO document why that works, as it breaks the
                 -- /geometric animation functions should return the same number of 'AnimatedPoint's at each iteration/ law.
                 | Interact
                 -- ^ The 'AnimatedPoint' can be mutated after an interaction
                 -- with the environment.
                 --
                 -- For the animation to be finite in time, 'AnimatedPoint's that
                 -- 'Interact' /must/ eventually be mutated by the environment.
                 --
                 -- Hence, assuming the environment has a finite size,
                 -- geometric animation functions returning 'AnimatedPoint's that
                 -- 'Interact' can guarantee animation finitude by computing their
                 -- coordinates using functions diverging in the 'Frame' argument.
                 deriving (Show, Eq)

-- | The result of an interaction between an 'AnimatedPoint' and the environment.
data InteractionResult = Mutation
                       -- ^ The 'AnimatedPoint' mutates, if it is allowed
                       -- to interact with the environment.
                       | Stable
                       -- ^ The 'AnimatedPoint' doesn't mutate.
                       deriving(Eq,Show)


-- TODO use this generalize animation chaining ?
{--
data Continuation = Continuation {
    _continuationFunction :: !(),
    _continuationOnWall :: !CanInteract
}
--}
