{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Animation.Design.Update
    ( updateAnimatedPoints
    ) where


import           Imj.Prelude

import           Imj.Animation.Design.Apply
import           Imj.Animation.Design.Types
import           Imj.Geo.Discrete
import           Imj.Iteration


{- | Given a length \( n \) list of animation functions, updates the \( n \)
first levels of an 'AnimatedPoints' using one animation function per level.

An 'AnimatedPoint' at level \( k <= n \) can mutate to an 'AnimatedPoints' by
interacting with its environment:

* if \( k = n \) , the new 'AnimatedPoints' will remain empty.
* if \( k < n \), the new 'AnimatedPoints' will be populated by 'AnimatedPoints'
using the \( k+1 \)th animation function.
-}
updateAnimatedPoints :: [Coords Pos -> Frame -> [AnimatedPoint]]
                     -- ^ The animation function at index @i@ updates
                     -- 'AnimatedPoints' at level @i@.
                     -> Frame
                     -- ^ Current iteration
                     -> (Coords Pos -> InteractionResult)
                     -- ^ Interaction function
                     -> AnimatedPoints
                     -> AnimatedPoints
updateAnimatedPoints [] _ _ aps = aps
updateAnimatedPoints (f:fs) globalFrame interaction (AnimatedPoints startFrame center branches) =
  let relativeFrame = globalFrame - startFrame
      branchesLevel1Mutated = updatePointsAndMutateIfNeeded f center relativeFrame interaction branches
      newBranches = map (\case
                            -- recurse for the 'AnimatedPoints's
                            Left aps -> Left $ updateAnimatedPoints fs relativeFrame interaction aps
                            -- the 'AnimatedPoint's are already up-to-date due to updatePointsAndMutateIfNeeded:
                            Right ap -> Right ap
                            ) branchesLevel1Mutated
  in AnimatedPoints startFrame center (Just newBranches)
