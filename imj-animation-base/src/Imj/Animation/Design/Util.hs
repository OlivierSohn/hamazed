{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Util
    (
    -- * Utilities
      earliestDeadline
    ) where

import           Imj.Prelude

import           Imj.Animation.Design.Types
import           Imj.Timing( KeyTime )

-- | Returns the earliest animation deadline
earliestDeadline :: [AnimationStep m] -> Maybe KeyTime
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      Just $ minimum $ map (\(AnimationStep deadline _ _ _) -> deadline) animations
