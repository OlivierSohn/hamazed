{-# OPTIONS_HADDOCK hide #-}

module Render.Backends.Internal.Clear
          ( clearIfNeeded
          ) where


import Render.Types
import Render.Backends.Internal.Types
import Render.Backends.Internal.Cells
import Render.Backends.Internal.Draw

clearIfNeeded :: ClearContext -> Buffers -> IO ()
clearIfNeeded ctxt b@(Buffers _ _ _ _ _ (Policies _ clearPolicy clearColor)) = do
  let clear = fillBackBuffer b (clearCell clearColor)
  case clearPolicy of
    ClearAtEveryFrame -> clear
    ClearOnAllocationOnly ->
      case ctxt of
        OnAllocation -> clear
        OnFrame -> return ()
