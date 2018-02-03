{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Cells
          ( clearCell
          , invalidCell
          ) where

import Data.Char(chr)
import Imj.Graphics.Color
import Imj.Graphics.Render.Delta.Internal.Types
import Imj.Graphics.Render.Delta.Cell

clearCell :: Color8 Background -> Cell
clearCell clearColor =
  -- Any foreground color would be ok
  mkCell (LayeredColor clearColor black) ' '

-- | A cell used to fill the front buffer when we don't know the current screen value.
--
-- Note that 0xFFFF is
-- <https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters not a character>.
invalidCell :: Cell
invalidCell = mkCell (LayeredColor red blue) (chr 0xFFFF)
