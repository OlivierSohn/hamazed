{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Cells
          (clearCell
          ) where

import Color
import Render.Types
import Render.Delta.Types
import Render.Delta.Cell

clearCell :: ClearColor -> Cell
clearCell clearColor =
  -- Any foreground color would be ok
  mkCell (LayeredColor clearColor white) ' '
