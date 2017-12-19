{-# LANGUAGE NoImplicitPrelude #-}

module Game.Deadline(
        Deadline(..)
      ) where

import           Imajuscule.Prelude

import           Game.Event( Step(..) )

import           Timing( KeyTime(..) )

data Deadline = Deadline {
    _deadlineTime :: !KeyTime
  , _deadlineType :: !Step
} deriving(Eq, Show)
