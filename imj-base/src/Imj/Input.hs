
{- | This module exports functions and types allowing to read player key-presses.

When we read a key with 'getKeyThenFlush' or 'tryGetKeyThenFlush',
we flush 'stdin' just after having read from it, to avoid repeated keys
slowing down the game.
-}
module Imj.Input
        ( module Imj.Input.Types
        , module Imj.Input.Blocking
        , module Imj.Input.NonBlocking
        ) where

import Imj.Input.Types
import Imj.Input.Blocking
import Imj.Input.NonBlocking
