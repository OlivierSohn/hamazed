{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Util
    (
      earliestDeadline
    ) where

import           Imajuscule.Prelude

import           Animation.Types

import           Timing( KeyTime )


earliestDeadline :: [Animation e] -> Maybe KeyTime
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      Just $ minimum $ map (\(Animation deadline _ _ _) -> deadline) animations
