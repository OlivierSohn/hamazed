{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Cells
          (clearCell
          ) where

import Color
import Render.Delta.Internal.Types
import Render.Delta.Cell

clearCell :: Color8 Background -> Cell
clearCell clearColor =
  -- Any foreground color would be ok
  mkCell (LayeredColor clearColor white) ' '
