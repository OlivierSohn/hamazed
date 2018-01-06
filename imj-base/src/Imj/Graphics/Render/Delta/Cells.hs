{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Cells
          (clearCell
          ) where

import Imj.Graphics.Color
import Imj.Graphics.Render.Delta.Internal.Types
import Imj.Graphics.Render.Delta.Cell

clearCell :: Color8 Background -> Cell
clearCell clearColor =
  -- Any foreground color would be ok
  mkCell (LayeredColor clearColor black) ' '
