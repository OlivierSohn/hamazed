{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Deadline(
        Deadline(..)
      ) where

import           Imj.Prelude

import           Imj.Game.Event( DeadlineType(..) )

import           Imj.Timing( KeyTime(..) )

data Deadline = Deadline {
    _deadlineTime :: !KeyTime
  , _deadlineType :: !DeadlineType
} deriving(Eq, Show)
