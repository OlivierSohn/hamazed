{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Buffers
          ( Buffers
          , IORef
          -- * Context
          -- ** Creation
          , newDefaultContext
          , newContext
          -- ** Configuration
          -- *** Resize Policy
          , setResizePolicy
          , ResizePolicy(..)
          -- *** Clear Policy
          , setClearPolicy
          , ClearPolicy(..)
          -- *** Clear color
          , setClearColor
          , ClearColor
          -- *** Stdout buffer mode
          , setStdoutBufferMode
          , BufferMode(..)
          -- ** Before quitting the game
          , restoreConsole
          -- utilities
          , updateSize
          ) where

import           Prelude hiding (replicate, unzip, length)

import           Data.IORef( IORef , newIORef , readIORef , writeIORef )
import           Data.Maybe( fromMaybe )
import           Data.Vector.Unboxed.Mutable( replicate, unzip, length )

import           System.IO(BufferMode(..), hSetBuffering, stdout)
import           Color

import           Render.Console
import           Render.Delta.Buffers.Dimensions
import           Render.Delta.Cell
import           Render.Delta.Cells
import qualified Render.Delta.UnboxedDynamic as Dyn (new)
import           Render.Types
import           Render.Delta.Types


-- we use IORef Buffers instead of Buffers because we want to update the size of the buffers
-- dynamically

-- | Equivalent to @newContext Nothing Nothing Nothing Nothing@
newDefaultContext :: IO (IORef Buffers)
newDefaultContext = newContext Nothing Nothing Nothing Nothing

-- | Creates a context using optional policies.
newContext :: Maybe ResizePolicy
           -> Maybe ClearPolicy
           -> Maybe ClearColor
           -> Maybe BufferMode -- ^ preferred stdout buffering (default gives a max size to stdout which should be optimal in most cases)
           -> IO (IORef Buffers)
newContext mayResizePolicy mayClearPolicy mayClearColor mayBufferMode = do
  let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
      clearColor = fromMaybe defaultClearColor mayClearColor
      stdoutBufMode = fromMaybe defaultStdoutMode mayBufferMode
  configureConsoleFor Gaming stdoutBufMode
  newContext' $ Policies resizePolicy clearPolicy clearColor


-- | restores stdin, stdout bufferings, unhides the cursor, restores echo for
--   stdin, restores the buffering of stdout to 'LineBuffering'
restoreConsole :: IO ()
restoreConsole =
 configureConsoleFor Editing LineBuffering

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
          -> IO (Buffer Back, Buffer Front, Delta, Dim Width)
mkBuffers width' height' backBufferCell = do
  let (sz, width) = bufferSizeFromWH width' height'
      (Color8Code bg, Color8Code fg, char) = expand backBufferCell
      -- We initialize to different colors to force a first render to the whole console.
      frontBufferCell = mkCell (LayeredColor (Color8Code (succ bg)) (Color8Code (succ fg))) (succ char)
  buf <- newBufferArray sz (backBufferCell, frontBufferCell)
  delta <- Dyn.new $ fromIntegral sz -- reserve the maximum possible size
  let (back, front) = unzip buf
  return (Buffer back, Buffer front, Delta delta, width)

defaultResizePolicy :: ResizePolicy
defaultResizePolicy = MatchTerminalSize

defaultClearColor :: Color8Code
defaultClearColor = black

defaultClearPolicy :: ClearPolicy
defaultClearPolicy = ClearAtEveryFrame

defaultStdoutMode :: BufferMode
defaultStdoutMode = BlockBuffering $ Just maxBound -- maximize the buffer size to avoid screen tearing

-- | Sets an optional 'ResizePolicy' or uses the default one when Nothing is passed.
--   If needed, the context will be resized at the end of the next 'flush' call.
setResizePolicy :: Maybe ResizePolicy -> IORef Buffers -> IO ()
setResizePolicy mayResizePolicy ref =
  readIORef ref
    >>= \(Buffers a b d e (Policies _ f g)) -> do
      let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      writeIORef ref $ Buffers a b d e (Policies resizePolicy f g)

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

-- | Sets 'ClearPolicy' to use for a context.
--   Default policy is used when Nothing is passed.
setClearPolicy :: Maybe ClearPolicy -> IORef Buffers -> IO ()
setClearPolicy mayClearPolicy ref =
  readIORef ref
    >>= \(Buffers a b d e (Policies f _ clearColor)) -> do
      let clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
          buffers = Buffers a b d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers

-- | Sets 'ClearColor' to use for a context.
--   Default color (black) is used when Nothing is passed.
setClearColor :: Maybe ClearColor -> IORef Buffers -> IO ()
setClearColor mayClearColor ref =
  readIORef ref
    >>= \(Buffers a b d e (Policies f clearPolicy _)) -> do
      let clearColor = fromMaybe defaultClearColor mayClearColor
          buffers = Buffers a b d e (Policies f clearPolicy clearColor)
      writeIORef ref buffers

-- | The default value (Nothing) maximizes the size stdout buffer.
setStdoutBufferMode :: Maybe BufferMode -> IO ()
setStdoutBufferMode mayBufferMode =
  hSetBuffering stdout (fromMaybe defaultStdoutMode mayBufferMode)

-- TODO use phantom types for Cell (requires Data.Vector.Unboxed.Deriving to use newtype in vector)
newBufferArray :: Dim Size -> (Cell, Cell) -> IO BackFrontBuffer
newBufferArray size = replicate (fromIntegral size)
