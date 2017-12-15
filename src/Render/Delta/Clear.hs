{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Clear
          ( clearIfNeeded
          ) where


import Render.Types
import Render.Delta.Types
import Render.Delta.Cells
import Render.Delta.Draw

clearIfNeeded :: ClearContext -> Buffers -> IO ()
clearIfNeeded ctxt b@(Buffers _ _ _ _ (Policies _ clearPolicy clearColor)) = do
  let clear = fillBackBuffer b (clearCell clearColor)
  case clearPolicy of
    ClearAtEveryFrame -> clear
    ClearOnAllocationOnly ->
      case ctxt of
        OnAllocation -> clear
        OnFrame -> return ()
