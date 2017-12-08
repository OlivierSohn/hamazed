{-# LANGUAGE NoImplicitPrelude #-}

-- Initial code from Rafael Ibraim : https://gist.github.com/ibraimgm/40e307d70feeb4f117cd
--
-- With the following modifications:
--   - in blitBuffer I removed the code that moves the cursor, as I think it's another concern
--      that should be handled by the user of this module using System.Console.ASCII.hideCursor)
--   - introduce bPutCharRaw that doesn't change the position in the buffer (to optimize
--       when moving the position is not needed)
--   - add the function bClear that clears the buffer using the initial color
--   - add a Bool parameter to blitBuffer to say if we want to clear the source buffer:
--      it might be faster to clear while blitting, instead of calling bClear afterwards,
--      because the values will probably be in a closer cache.
--   - introduce modOptimized to optimize modulos, because most of the time,
--      the value is returned unchanged so a simple comparison is enough.
--   - Reworked logic of drawCell / applyBuffer to draw only when strictly required.
--     To that end, we keep track of the current color of the console while drawing.
--   - Add support for 8-bits ANSI colors
--   - Optimize memory layout

module Render.Backends.Internal.Delta
       ( bSetRenderSize
       , bGetRenderSize
       , bSetForeground
       , bSetBackground
       , bSetColors
       , bGotoXY
       , bPutChar
       , bPutCharRaw
       , bPutChars
       , bPutStr
       , bPutText
       , bClear
       , blitBuffer
       -- reexports from System.Console.ANSI
       , Color8Code(..)
       ) where

import           Imajuscule.Prelude hiding (replicate)

import qualified Prelude ( putChar, putStr )

import           Data.Vector.Unboxed.Mutable( IOVector, replicate, read, write, unzip )

import           Data.Colour.SRGB( RGB(..) )
import           Data.IORef( IORef , newIORef , readIORef , writeIORef )
import           Data.String( String )
import           Data.Text( Text, unpack )
import           Data.Word( Word32, Word16 )

import           System.Console.ANSI( xterm256ColorToCode
                                    , Color8Code(..)
                                    , Xterm256Color(..)
                                    , setCursorPosition
                                    , setSGRCode
                                    , SGR(..)
                                    , ConsoleLayer(..) )
import           System.IO.Unsafe( unsafePerformIO )

import           Render.Backends.Internal.BufferCell

-- type definitions and global instance

data Colors = Colors {
    _colorsForeground :: {-# UNPACK #-} !Color8Code
  , _colorsBackground :: {-# UNPACK #-} !Color8Code
}

type BackFrontBuffer = IOVector (Cell, Cell)

newtype Buffer a = Buffer (IOVector Cell)

-- | Buffer types
data Back
data Front

-- that could fit in a Word64 (Color8Code = 2 Word8, index = Word32)
-- with 16 bits extra room
data Pencil = Pencil {
    _pencilBufferIndex :: !Word32
  , _pencilForeground :: !Color8Code
  , _pencilBackground :: !Color8Code
}

-- that could fit exactly in a Word64 (width and height = 2 Word16, size = Word32)
data Buffers = Buffers {
    _renderStateBackBuffer :: !(Buffer Back)
  , _renderStateFrontBuffer :: !(Buffer Front)
  , _buffersSize :: !Word32
  , _buffersWidth :: !Word16
  , _buffersHeight :: !Word16
}


bGetRenderSize :: IO (Word16, Word16)
bGetRenderSize = do
  (RenderState _ (Buffers _ _ _ w h)) <- readIORef screenBuffer
  return (w,h)

bSetRenderSize :: Word16 -> Word16 -> IO ()
bSetRenderSize width height = do
  (RenderState (Pencil idx fg bg) (Buffers _ _ _ prevWidth prevHeight)) <- readIORef screenBuffer
  if prevWidth == width && prevHeight == height
    then
      return ()
    else do
      buffers@(Buffers _ _ size _ _) <- mkBuffers width height
      let newPencilIdx = idx `fastMod` size
      writeIORef screenBuffer $ RenderState (Pencil newPencilIdx fg bg) buffers

-- | creates buffers for given width and height, replaces 0 width or height by 1.
mkBuffers :: Word16 -> Word16 -> IO Buffers
mkBuffers width' height' = do
  let width = max 1 width'
      height = max 1 height'
      sz = fromIntegral width * fromIntegral height
  -- We initialize to different colors so that in first render the whole console is drawn to.
  buf <- newBufferArray sz (initialCell, nocolorCell)
  let (back, front) = unzip buf
  return $ Buffers (Buffer back) (Buffer front) sz width height

data RenderState = RenderState {
    _renderStatePencil :: !Pencil
  , _renderStateBuffers :: !Buffers
}

-- TODO use phantom types for Cell (requires Data.Vector.Unboxed.Deriving to use newtype in vector)
newBufferArray :: Word32 -> (Cell, Cell) -> IO BackFrontBuffer
newBufferArray size = replicate (fromIntegral size)

-- TODO caller should be able to set this
initialForeground :: Color8Code
initialForeground = xterm256ColorToCode $ RGBColor $ RGB 5 5 5

initialBackground :: Color8Code
initialBackground = xterm256ColorToCode $ RGBColor $ RGB 0 0 0

noColor :: Color8Code
noColor = Color8Code (-1)

{-# INLINE initialCell #-}
initialCell :: Cell
initialCell =
  let res = 0x200000E710 -- optimization for 'drawDelta' (TODO check if the compiler would have found it)
  in assert (mkCell initialForeground initialBackground ' ' == res) res

nocolorCell :: Cell
nocolorCell = mkCell noColor noColor ' '

{-# NOINLINE screenBuffer #-}
screenBuffer :: IORef RenderState
screenBuffer =
  unsafePerformIO $ do
    buffers <- mkBuffers 0 0
    newIORef (RenderState mkInitialState buffers)

mkInitialState :: Pencil
mkInitialState = Pencil 0 initialForeground initialBackground

-- | Modulo optimized for cases where most of the time,
--    a < b (for a mod b)
{-# INLINE fastMod #-}
fastMod :: Word32 -> Word32 -> Word32
fastMod a b
  | 0 <= a && a < b = a          -- fast path
  | otherwise       = a `mod` b  -- slow path

{-# INLINE positionFromXY #-}
positionFromXY :: Buffers -> Word16 -> Word16 -> Word32
positionFromXY (Buffers _ _ size width _) x y =
  (fromIntegral y * fromIntegral width + fromIntegral x) `fastMod` size

{-# INLINE xyFromPosition #-}
xyFromPosition :: Buffers -> Word32 -> (Word16, Word16)
xyFromPosition (Buffers _ _ size width _) pos =
  (fromIntegral x, fromIntegral y)
  where
    pos' = pos `fastMod` size
    x = pos' - fromIntegral y * fromIntegral width
    y = pos' `div` fromIntegral width


bSetForeground :: Color8Code -> IO Color8Code
bSetForeground fg = do
  (RenderState (Pencil idx prevFg prevBg) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx fg prevBg) b
  return prevFg

bSetBackground :: Color8Code -> IO Color8Code
bSetBackground bg = do
  (RenderState (Pencil idx prevFg prevBg) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx prevFg bg) b
  return prevBg

bSetColors :: (Color8Code, Color8Code) -> IO (Color8Code, Color8Code)
bSetColors (fg,bg) = do
  (RenderState (Pencil idx prevFg prevBg) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx fg bg) b
  return (prevFg, prevBg)


bGotoXY :: Word16 -> Word16 -> IO ()
bGotoXY x y = do
  (RenderState (Pencil _ fg bg) b) <- readIORef screenBuffer
  let idx = positionFromXY b x y
  writeIORef screenBuffer $ RenderState (Pencil idx fg bg) b

-- | Write a char and return the position of the written char in the buffer
bPutCharRaw :: Char -> IO ()
bPutCharRaw c = do
  (RenderState (Pencil idx fg bg) (Buffers back _ _ _ _)) <- readIORef screenBuffer
  writeToBack back idx $ mkCell fg bg c

bPutChars :: Word32 -> Char -> IO ()
bPutChars count c = do
  (RenderState (Pencil idx fg bg) (Buffers back _ size _ _)) <- readIORef screenBuffer
  let cell = mkCell fg bg c
  mapM_ (\i -> let pos = (idx + i) `fastMod` size
               in writeToBack back pos cell) [0..pred count]

-- | Write a char and advance in the buffer
bPutChar :: Char -> IO ()
bPutChar c = do
  bPutCharRaw c
  (RenderState (Pencil idx fg bg) b@(Buffers _ _ size _ _)) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil (succ idx `fastMod` size) fg bg) b

bPutStr :: String -> IO ()
bPutStr = mapM_ bPutChar

bPutText :: Text -> IO ()
bPutText text =
  mapM_ bPutChar (unpack text)

{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Word32 -> Cell -> IO ()
writeToBack (Buffer b) pos = write b (fromIntegral pos)

bClear :: IO ()
bClear = do
  (RenderState _ b) <- readIORef screenBuffer
  fillBackBuffer b initialCell

fillBackBuffer :: Buffers -> Cell -> IO ()
fillBackBuffer (Buffers (Buffer b) _ size _ _) cell =
  mapM_ (\pos -> write b pos cell) [0..fromIntegral $ pred size]

-- blit the backbuffer into the main buffer and change the screen
blitBuffer :: Bool
           -- ^ Clear the source buffer
           -> IO ()
blitBuffer clearSource = do
  (RenderState _ buffers) <- readIORef screenBuffer
  drawDelta buffers 0 clearSource Nothing

drawDelta :: Buffers
          -> Word32
          -- ^ the position in the buffer
          -> Bool
          -- ^ should we clear the buffer containing the new values?
          -> Maybe Colors
          -- ^ The current console color, if it is known
          -> IO ()
drawDelta b@(Buffers (Buffer backBuf) (Buffer frontBuf) size _ _) idx clearBack mayCurrentConsoleColor
  | idx == size = return ()
  | otherwise = do
      let i = fromIntegral idx
      valueToDisplay <- read backBuf i
      valueCurrentlyDisplayed <- read frontBuf i
      mayNewConsoleColor <-
        if valueToDisplay == valueCurrentlyDisplayed
          then
            return Nothing
          else do
            -- draw on screen
            res <- drawCell b idx valueToDisplay mayCurrentConsoleColor
            -- update front buffer with drawn value
            write frontBuf i valueToDisplay
            return $ Just res
      when (clearBack && (initialCell /= valueToDisplay)) $ write backBuf i initialCell
      drawDelta b (succ idx) clearBack (mayNewConsoleColor <|> mayCurrentConsoleColor)

drawCell :: Buffers -> Word32 -> Cell -> Maybe Colors -> IO Colors
drawCell b idx cell maybeCurrentConsoleColor = do
  let (fg, bg, char) = expand cell
      (fgChange, bgChange) = maybe (True, True) (\(Colors fg' bg') -> (fg'/=fg, bg'/=bg)) maybeCurrentConsoleColor
      sgrs = [SetPaletteColor Foreground fg | fgChange] ++
             [SetPaletteColor Background bg | bgChange]
  -- move to drawing position
  setCursorPositionFromBufferIdx b idx
  if fgChange || bgChange
    then
      Prelude.putStr $ setSGRCode sgrs ++ [char]
    else
      Prelude.putChar char
  return $ Colors fg bg

{-# INLINE setCursorPositionFromBufferIdx #-}
setCursorPositionFromBufferIdx :: Buffers -> Word32 -> IO ()
setCursorPositionFromBufferIdx b pos = do
  let (x, y) = xyFromPosition b pos
  setCursorPosition (fromIntegral y) (fromIntegral x)
