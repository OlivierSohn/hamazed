
module Render.Backends.Internal.Cells 
          (clearCell
          ) where

import Color
import Render.Types
import Render.Backends.Internal.Types
import Render.Backends.Internal.Cell

clearCell :: ClearColor -> Cell
clearCell clearColor =
  -- Any foreground color would be ok
  mkCell (LayeredColor clearColor white) ' '
