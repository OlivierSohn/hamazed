{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Render.Delta.Clear
          ( clearIfNeeded
          ) where

import           Imj.Prelude

import           Control.Monad(when)

import           Imj.Render.Delta.Cells
import           Imj.Render.Delta.Draw
import           Imj.Render.Delta.Internal.Types
import           Imj.Render.Delta.Types

clearIfNeeded :: ClearContext -> Buffers -> IO ()
clearIfNeeded context b@(Buffers _ _ _ _ (Policies _ clearPolicy clearColor)) =
  when (clearPolicy == ClearAtEveryFrame || context == OnAllocation) $
    fillBackBuffer b (clearCell clearColor)
