{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render.Delta.Buffers
          ( Buffers
          , IORef
          , newContext
          -- utilities
          , updateSize
          ) where

import           Prelude hiding (replicate, unzip, length)

import           Data.IORef( IORef , newIORef , readIORef , writeIORef )
import           Data.Maybe( fromMaybe )
import           Data.Vector.Unboxed.Mutable( replicate, unzip, length )

import           Imj.Graphics.Color.Types

import           Imj.Graphics.Render.Delta.Buffers.Dimensions
import           Imj.Graphics.Render.Delta.Cell
import           Imj.Graphics.Render.Delta.Cells
import qualified Imj.Graphics.Render.Delta.DynUnboxedVec as Dyn (new)
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.DefaultPolicies
import           Imj.Graphics.Render.Delta.Internal.Types


-- we use IORef Buffers instead of Buffers because we want to update the size of the buffers
-- dynamically

-- Creates a context using optional policies.
newContext :: Maybe ResizePolicy
           -> Maybe ClearPolicy
           -> Maybe (Color8 Background)
           -> IO (IORef Buffers)
newContext mayResizePolicy mayClearPolicy mayClearColor = do
  let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
      clearColor = fromMaybe defaultClearColor mayClearColor
  newContext' $ Policies resizePolicy clearPolicy clearColor

newContext' :: Policies -> IO (IORef Buffers)
newContext' policies@(Policies resizePolicy _ _) =
  getDimensions resizePolicy
    >>= uncurry (createBuffers policies)
      >>= newIORef

-- | Creates buffers for given width and height, replaces 0 width or height by 1.
mkBuffers :: Dim Width
          -> Dim Height
          -> Cell
          -> IO (Buffer Back, Buffer Front, Delta, Dim Width)
mkBuffers width' height' backBufferCell = do
  let (sz, width) = bufferSizeFromWH width' height'
      (bg, fg, char) = expand backBufferCell
      -- We initialize to different colors to force a first render to the whole console.
      frontBufferCell = mkCell (LayeredColor (succ bg) (succ fg)) (succ char)
  buf <- newBufferArray sz (backBufferCell, frontBufferCell)
  delta <- Dyn.new $ fromIntegral sz -- reserve the maximum possible size
  let (back, front) = unzip buf
  return (Buffer back, Buffer front, Delta delta, width)

adjustSizeIfNeeded :: Buffers -> IO Buffers
adjustSizeIfNeeded buffers@(Buffers (Buffer back) _ prevWidth _ policies@(Policies resizePolicy _ _)) = do
  (width, height) <- getDimensions resizePolicy
  let prevSize = fromIntegral $ length back
      prevHeight = getHeight prevWidth prevSize
  if prevWidth /= width || prevHeight /= height
    then
      createBuffers policies width height
    else
      return buffers

createBuffers :: Policies -> Dim Width -> Dim Height -> IO Buffers
createBuffers pol@(Policies _ _ clearColor) w h = do
  (newBack, newFront, newDelta, newWidth) <- mkBuffers w h (clearCell clearColor)
  -- no need to clear : we initialized with the right value
  return $ Buffers newBack newFront newWidth newDelta pol

updateSize :: IORef Buffers -> IO ()
updateSize ref =
  readIORef ref >>= adjustSizeIfNeeded >>= writeIORef ref

-- TODO use phantom types for Cell (requires Data.Vector.Unboxed.Deriving to use newtype in vector)
newBufferArray :: Dim BufferSize -> (Cell, Cell) -> IO BackFrontBuffer
newBufferArray size = replicate (fromIntegral size)
