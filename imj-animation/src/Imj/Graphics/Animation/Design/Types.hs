{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Design.Types
          ( Animation(..)
          , EnvFunctions(..)
          , Distance(..)
          , UpdateSpec(..)
          , AnimatedPoints(..)
          , AnimatedPoint(..)
          , CanInteract(..)
          , InteractionResult(..)
          -- Reexports
          , VecPosSpeed
          ) where

import           Imj.Prelude

import           GHC.Show(showString)

import           Imj.Iteration
import           Imj.Geo.Discrete.Types
import           Imj.Physics.Continuous.Types
import           Imj.Timing


data Animation = Animation {
    _animationPoints :: !AnimatedPoints
    -- ^ The current points.
  , _animationUpdate :: !(EnvFunctions
                      -> Frame
                      -> AnimatedPoints
                      -> AnimatedPoints)
    -- ^ The function updating 'AnimatedPoints'.
  , _animationEnvFunctions :: !EnvFunctions
    -- ^ The environment functiosn.
  , _animationNextUpdateSpec :: !UpdateSpec
    -- ^ The time and iteration of the next update
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to draw animated points when the 'AnimatedPoints'
    -- don't specify one. If 'Nothing', the animation function /must/ specify one
    -- when creating new 'AnimatedPoint's.
}

-- | Functions to interact with the environment
data EnvFunctions = EnvFunctions {
    _envFunctionsInteract :: !(Coords Pos -> InteractionResult)
    {- ^ /Interaction/ function.

    During update, 'AnimatedPoint's for which this returns 'Mutation' can mutate
    if they are allowed to.

    During draw, 'AnimatedPoint's for which this
    function returns 'Stable' are drawn. Others are
    not drawn because they would overlap with the environment. -}
  , _envFunctionsDistance :: !(Vec2 Pos -> Distance)
    {- ^ /Distance/ function.

    During update, 'AnimatedPoint's which 'CanInteract' with the environment, and
    for which this function returns 'TooFar' are removed from the animation
    (i.e, they are converted to an 'AnimatedPoints' at their location,
    and this 'AnimatedPoints' will not evolve because its center, too will be 'TooFar'.).

    Note that 'AnimatedPoint's that 'DontInteract' with the environment will not
    be affected. As of today, animation functions generating 'AnimatedPoint's that
    'DontInteract' don't diverge, so it's not an issue. In case new types of animation
    functions are developped, we could change this behaviour. -}
}

data Distance = DistanceOK
              | TooFar
              -- ^ When an 'AnimatedPoint' is too far, the animation removes it.
              deriving(Show)


instance Show Animation where
  showsPrec _ (Animation a _ _ b c) =
    showString $ "Animation{" ++ show (a,b,c) ++ "}"


data UpdateSpec = UpdateSpec {
    _updateSpecTime :: !KeyTime
    -- ^ The time at which the update should happen.
  , _updateSpecIteration :: !Iteration
    -- ^ The iteration that will be used in the update.
} deriving(Show)


data AnimatedPoints = AnimatedPoints {
    _animatedPointsBranches :: !(Maybe [Either AnimatedPoints AnimatedPoint])
    -- ^ The children. A child can be 'Right' 'AnimatedPoint' or 'Left' 'AnimatedPoints'.
    --
    --  When a 'Right' 'AnimatedPoint' mutates, it is converted to an empty 'Left' 'AnimatedPoints'
  , _animatedPointsCenter :: !VecPosSpeed
    -- ^ The center, aka the position and speed of the 'AnimatedPoint', w.r.t the animation reference frame,
    -- that gave birth to this 'AnimatedPoints'.
  , _animatedPointsFrame :: !Frame
    -- ^ The frame at which this 'AnimatedPoints' was created, relatively to the parent, if any.
} deriving (Show)

data AnimatedPoint = AnimatedPoint {
    _animatedPointCanInteract :: !CanInteract
    -- ^ Can the point interact with the environment?
  , _animatedPointVecPosSpeed :: {-# UNPACK #-} !VecPosSpeed
    -- ^ Continuous location and speed, w.r.t the animation reference frame.
  , _animatedPointDrawnWith :: !(Maybe Char)
    -- ^ The char used to draw it. If 'Nothing', 'Animation' /must/ specify a 'Char'.
} deriving (Show)

data CanInteract = DontInteract
                 {- ^ The 'AnimatedPoint' can't interact with the environment.

                 To ensure that the 'Animation' is finite in time,
                 animation functions returning 'AnimatedPoint's that
                 'DontInteract' should return an empty list of 'AnimatedPoint's
                 for each 'Frame' after a given 'Frame'. -}
                 | Interact
                 {- ^ The 'AnimatedPoint' can be mutated after an interaction
                 with the environment.

                 For the animation to be finite in time, 'AnimatedPoint's that
                 'Interact' /must/ eventually be either mutated by the environment
                 or be declared 'TooFar'.

                 Hence, animation functions returning 'AnimatedPoint's that
                 'Interact' can guarantee animation finitude by computing their
                 coordinates using functions /diverging/ in the 'Frame' argument:

                 * if the environment has a finite size, they will eventually mutate
                 * if the environment is infinite, we rely on the distance function in 'EnvFunctions'
                 to eventually return 'TooFar'. -}
                 deriving (Show, Eq)

data InteractionResult = Mutation
                       -- ^ The 'AnimatedPoint' can mutate.
                       | Stable
                       -- ^ The 'AnimatedPoint' doesn't mutate.
                       deriving(Eq,Show)
