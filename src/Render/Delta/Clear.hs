{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Clear
          ( clearIfNeeded
          ) where

import           Control.Monad(when)
import           Render.Delta.Types
import           Render.Delta.Cells
import           Render.Delta.Draw
import           Render.Types

clearIfNeeded :: ClearContext -> Buffers -> IO ()
clearIfNeeded context b@(Buffers _ _ _ _ (Policies _ clearPolicy clearColor)) =
  when (clearPolicy == ClearAtEveryFrame || context == OnAllocation) $
    fillBackBuffer b (clearCell clearColor)
