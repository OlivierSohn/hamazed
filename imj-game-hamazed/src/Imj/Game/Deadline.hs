{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Deadline(
        Deadline(..)
      ) where

import           Imj.Prelude

import           Imj.Game.Event( Step(..) )

import           Imj.Timing( KeyTime(..) )

data Deadline = Deadline {
    _deadlineTime :: !KeyTime
  , _deadlineType :: !Step
} deriving(Eq, Show)
