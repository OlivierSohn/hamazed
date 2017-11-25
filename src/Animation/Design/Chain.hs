{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Design.Chain
    (
      chainOnCollision
    ) where


import           Imajuscule.Prelude

import           Animation.Design.Apply
import           Animation.Types
import           Geo( Coords )
import           WorldSize( Location )


chainOnCollision :: (Coords -> Frame -> [Coords])
                 -- ^ animation 1
                 -> (Coords -> Frame -> [Coords])
                 -- ^ animation 2
                 -> Iteration
                 -> (Coords -> Location)
                 -- ^ collision function
                 -> Tree
                 -> Tree
chainOnCollision anim1 anim2 iteration getLocation tree  =
  let (Tree a b branches) = applyAnimation anim1 iteration getLocation tree
      newBranches = Just $ case branches of
        Nothing -> error "applyAnimation was supposed to create a Just ?"
        Just l ->  map (either (Left . applyAnimation anim2 iteration getLocation) Right) l
  in Tree a b newBranches

-- TODO generic chaining of animations
{--
chainAnimationsOnCollision :: [Coords -> Iteration -> [Coords]]
                           -- ^ each animation function should return a constant number of Coords across iterations
                           -> Iteration
                           -> (Coords -> Location)
                           -- ^ collision function
                           -> Tree
                           -> Tree
chainAnimationsOnCollision animations iteration getLocation tree = undefined
--}
