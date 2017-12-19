{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Util
    (
      earliestDeadline
    ) where

import           Imajuscule.Prelude

import           Animation.Types

import           Timing( KeyTime )


earliestDeadline :: [AnimationUpdate m] -> Maybe KeyTime
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      Just $ minimum $ map (\(AnimationUpdate deadline _ _ _) -> deadline) animations
