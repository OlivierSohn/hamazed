{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Util
    (
      earliestDeadline
    ) where

import           Imj.Prelude

import           Imj.Animation.Types

import           Imj.Timing( KeyTime )


earliestDeadline :: [AnimationUpdate m] -> Maybe KeyTime
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      Just $ minimum $ map (\(AnimationUpdate deadline _ _ _) -> deadline) animations
