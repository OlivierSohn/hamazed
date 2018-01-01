
{- | These module export functions and types related to reading the player key-presses.

After reading a key in 'getKeyThenFlush' or 'tryGetKeyThenFlush',
we flush 'stdin' to avoid repeated keys slowing down the game.
-}
module Imj.Input
        ( module Imj.Input.Types
        , module Imj.Input.Blocking
        , module Imj.Input.NonBlocking
        ) where

import Imj.Input.Types
import Imj.Input.Blocking
import Imj.Input.NonBlocking
