{-# OPTIONS_HADDOCK hide #-}

module Render.Backends.Internal.Buffers
          ( Buffers
          -- * Context
          -- ** Creation
          , newDefaultContext
          , newContext
          -- ** Policies
          , setResizePolicy
          , setClearPolicy
          -- utilities
          , updateSize
          ) where

import           Prelude hiding (replicate, unzip)

import           Data.IORef( IORef , newIORef , readIORef , writeIORef )
import           Data.Maybe( fromMaybe )
import           Data.Vector.Unboxed.Mutable( replicate, unzip )

import           Color

import           Render.Backends.Internal.Buffers.Dimensions
import           Render.Backends.Internal.Cell
import           Render.Backends.Internal.Cells
import qualified Render.Backends.Internal.UnboxedDynamic as Dyn (new)
import           Render.Types
import           Render.Backends.Internal.Types


-- | Equivalent to @newContext Nothing Nothing Nothing@
newDefaultContext :: IO (IORef Buffers)
newDefaultContext = newContext Nothing Nothing Nothing

-- | Creates a context using optional policies.
newContext :: Maybe ResizePolicy -> Maybe ClearPolicy -> Maybe ClearColor -> IO (IORef Buffers)
newContext mayResizePolicy mayClearPolicy mayClearColor = do
  let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
      clearColor = fromMaybe defaultClearColor mayClearColor
  newContext' $ Policies resizePolicy clearPolicy clearColor

newContext' :: Policies -> IO (IORef Buffers)
newContext' policies@(Policies resizePolicy _ _) =
  getDimensions resizePolicy
    >>=
      uncurry (createBuffers policies)
        >>=
          newIORef

-- | Creates buffers for given width and height, replaces 0 width or height by 1.
mkBuffers :: Dim Width
          -> Dim Height
          -> Cell
          -> IO (Buffer Back, Buffer Front, Delta, Dim Size, Dim Width)
mkBuffers width' height' backBufferCell = do
  let (sz, width) = bufferSizeFromWH width' height'
      (Color8Code bg, Color8Code fg, char) = expand backBufferCell
      -- We initialize to different colors to force a first render to the whole console.
      frontBufferCell = mkCell (LayeredColor (Color8Code (succ bg)) (Color8Code (succ fg))) (succ char)
  buf <- newBufferArray sz (backBufferCell, frontBufferCell)
  delta <- Dyn.new $ fromIntegral sz -- reserve the maximum possible size
  let (back, front) = unzip buf
  return (Buffer back, Buffer front, Delta delta, sz, width)

defaultResizePolicy :: ResizePolicy
defaultResizePolicy = MatchTerminalSize

defaultClearColor :: Color8Code
defaultClearColor = black

defaultClearPolicy :: ClearPolicy
defaultClearPolicy = ClearAtEveryFrame

-- | Sets an optional 'ResizePolicy' or uses the default one when Nothing is passed.
--   If needed, the context will be resized at the end of the next 'flush' call.
setResizePolicy :: IORef Buffers -> Maybe ResizePolicy -> IO ()
setResizePolicy ref mayResizePolicy =
  readIORef ref
    >>= \(Buffers a b c d e (Policies _ f g)) -> do
      let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      writeIORef ref $ Buffers a b c d e (Policies resizePolicy f g)

adjustSizeIfNeeded :: Buffers -> IO Buffers
adjustSizeIfNeeded buffers@(Buffers _ _ prevSize prevWidth _ policies@(Policies resizePolicy _ _)) = do
  (width, height) <- getDimensions resizePolicy
  let prevHeight = getHeight prevWidth prevSize
  if prevWidth /= width || prevHeight /= height
    then
      createBuffers policies width height
    else
      return buffers

createBuffers :: Policies -> Dim Width -> Dim Height -> IO Buffers
createBuffers pol@(Policies _ _ clearColor) w h = do
  (newBack, newFront, newDelta, newSize, newWidth) <- mkBuffers w h (clearCell clearColor)
  -- no need to clear : we initialized with the right value
  return $ Buffers newBack newFront newSize newWidth newDelta pol

updateSize :: IORef Buffers -> IO ()
updateSize ref =
  readIORef ref >>= adjustSizeIfNeeded >>= writeIORef ref

-- | Sets 'ClearPolicy' and 'ClearColor' to use for a context. Default policy or Colors are used when Nothing is passed.
setClearPolicy :: IORef Buffers -> Maybe ClearPolicy -> Maybe ClearColor -> IO ()
setClearPolicy ref mayClearPolicy mayClearColor =
  readIORef ref
    >>= \(Buffers a b c d e (Policies f _ _)) -> do
      let clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
          clearColor = fromMaybe defaultClearColor mayClearColor
          buffers = Buffers a b c d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers

-- TODO use phantom types for Cell (requires Data.Vector.Unboxed.Deriving to use newtype in vector)
newBufferArray :: Dim Size -> (Cell, Cell) -> IO BackFrontBuffer
newBufferArray size = replicate (fromIntegral size)
