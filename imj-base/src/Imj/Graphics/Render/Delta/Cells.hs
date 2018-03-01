{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Cells
          ( clearCell
          , invalidCell
          ) where

import Imj.Prelude
import Data.Char(chr)

import Imj.Graphics.Color
import Imj.Graphics.Font
import Imj.Graphics.Render.Delta.Internal.Types
import Imj.Graphics.Render.Delta.Cell

clearCell :: Color8 Background -> Cell
clearCell clearColor =
  -- Any foreground color would be ok.
  -- Any font can be used.
  mkCell (LayeredColor clearColor black) $ textGlyph ' '

-- | A cell used to fill the front buffer when we don't know the current screen value.
--
-- Note that 0xFFFF is
-- <https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters not a character>.
invalidCell :: Cell
invalidCell = mkCell (LayeredColor red blue) $ textGlyph (chr 0xFFFF)
