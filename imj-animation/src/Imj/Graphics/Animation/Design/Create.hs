{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Design.Create
    ( mkAnimation
    ) where


import           Imj.Prelude

import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Animation.Design.Update
import           Imj.Graphics.Animation.Design.UpdateAnimatedPoints
import           Imj.Iteration
import           Imj.Timing

-- | Creates an animation and initializes it by updating it once.
mkAnimation :: Coords Pos
            -- ^ Center of the root 'AnimatedPoints'.
            -> [Coords Pos -> Frame -> [AnimatedPoint]]
            {- ^ List of /animation functions/. Every /animation function/ generates
            'AnimatedPoint's for a given level of the root 'AnimatedPoints':

            The k-th /animation function/ generates 'AnimatedPoints'
            living in the (k+2)-th level, and takes the center of a (k+1)-th
            level 'AnimatedPoints' in its 'Coords' argument.

            /Animation functions/ are expected to return the same number of 'AnimatedPoint's
            at each iteration, or none after a given iteration to indicate that
            their animation is over. -}
            -> Speed
            -- ^ Animation discrete speed. Tells by how much the 'Frame', passed
            -- to animation functions, is incremented during an update.
            -> (Coords Pos -> InteractionResult)
            {- ^ The environmental interaction function.

            During update, 'AnimatedPoint's for which this function returns
            'Mutation' can mutate if they are allowed to.

            During render, 'AnimatedPoint's for which this
            function returns 'Stable' are drawn. Others are
            not drawn because they would overlap with the environment. -}
            -> Either SystemTime KeyTime
            -- ^ 'Right' 'KeyTime' of the event's deadline
            -- that triggered this animation, or 'Left' 'SystemTime'
            -- of the current time if a player action triggered this animation
            -> Maybe Char
            {- ^ The default 'Char' to draw an 'AnimatedPoint' with, if an 'AnimatedPoint'
            of that 'Animation' doesn't specify one (i.e if one of the /animation functions/
            passed as argument don't set the 'Char' of the 'AnimatedPoint' they generate). -}
            -> Maybe Animation
            -- ^ Depending on /animation functions/, the created 'Animation' may be over
            -- after the first update, hence 'Nothing' would be returned.
mkAnimation center updates speed interaction t' mayChar =
  let update = updateAnimatedPoints updates
      t = either KeyTime id t'
      u = UpdateSpec t (zeroIteration speed)
      points = mkAnimatedPoints center
  in updateAnimationIfNeeded t $ Animation points update interaction u mayChar


-- | Constructs an 'AnimatedPoints'.
mkAnimatedPoints :: Coords Pos
                 -- ^ Where the first animation should start.
                 -> AnimatedPoints
mkAnimatedPoints center =
  AnimatedPoints Nothing center 0
