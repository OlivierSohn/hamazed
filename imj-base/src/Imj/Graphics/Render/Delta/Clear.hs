{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Clear
          ( clearIfNeeded
          ) where

import           Imj.Prelude

import           Imj.Graphics.Render.Delta.Internal.Types
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Cells
import           Imj.Graphics.Render.Delta.Draw

clearIfNeeded :: ClearContext -> Buffers -> IO ()
clearIfNeeded context b@(Buffers _ _ _ _ _ (Policies _ clearPolicy clearColor)) =
  when (clearPolicy == ClearAtEveryFrame || context == OnAllocation) $
    fillBackBuffer b (clearCell clearColor)
