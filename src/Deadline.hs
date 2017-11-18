
module Deadline( Deadline(..)
) where

import           Imajuscule.Prelude

import           Timing( KeyTime(..) )
import           Event(Step(..))

data Deadline = Deadline {
    _deadlineTime :: !KeyTime
  , _deadlineType :: !Step
} deriving(Eq, Show)
