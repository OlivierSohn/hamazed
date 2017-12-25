
{- | When we read a key with 'getKeyThenFlush' or 'tryGetKeyThenFlush',
we flush 'stdin' just after having read from it, to avoid repeated keys
slowing down the game.
-}
module Imj.Key
        ( module Imj.Key.Types
        , module Imj.Key.Blocking
        , module Imj.Key.NonBlocking
        ) where

import Imj.Key.Types
import Imj.Key.Blocking
import Imj.Key.NonBlocking
