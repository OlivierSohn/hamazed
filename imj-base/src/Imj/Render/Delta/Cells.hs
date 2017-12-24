{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Render.Delta.Cells
          (clearCell
          ) where

import Imj.Color
import Imj.Render.Delta.Internal.Types
import Imj.Render.Delta.Cell

clearCell :: Color8 Background -> Cell
clearCell clearColor =
  -- Any foreground color would be ok
  mkCell (LayeredColor clearColor white) ' '
