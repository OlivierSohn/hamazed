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
       ( bSetForeground
       , bSetRawForeground
       , bSetBackground
       , bSetRawBackground
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
       , ColorIntensity(..)
       , Color(..)
       , Xterm256Color(..)
       , Color8Code(..)
       ) where

import           Imajuscule.Prelude hiding (replicate)

import qualified Prelude ( putChar, putStr )

import           Data.Vector.Unboxed.Mutable( IOVector, replicate, read, write, unzip )

import           Data.IORef( IORef
                           , newIORef
                           , readIORef
                           , writeIORef )
import           Data.String( String )
import           Data.Text( Text, unpack )
import           Data.Colour.SRGB(RGB(..))

import           System.Console.ANSI( ColorIntensity(..)
                                    , Color(..)
                                    , Xterm256Color(..)
                                    , xterm256ColorToCode
                                    , Color8Code(..)
                                    , setCursorPosition
                                    , setSGRCode
                                    , SGR(..)
                                    , ConsoleLayer(..)
                                    , colorToCode )
import           System.IO.Unsafe( unsafePerformIO )

import           Render.Backends.Internal.BufferCell

-- constant data
bufferWidth :: Int
bufferWidth = 300

bufferHeight :: Int
bufferHeight = 70

bufferMaxIdx :: Int
bufferMaxIdx = bufferSize - 1

bufferSize :: Int
bufferSize = bufferWidth * bufferHeight

-- type definitions and global instance

type Colors = (Color8Code, Color8Code) -- (foregroud color, background color)

type BackFrontBuffer = IOVector (Cell, Cell)

newtype Buffer a = Buffer (IOVector Cell)

-- | Buffer types
data Back
data Front

data Pencil = Pencil {
    _pencilBufferIndex :: !Int
  , _pencilForeground :: !Color8Code
  , _pencilBackground :: !Color8Code
}

data RenderState = RenderState {
    _renderStatePencil :: !Pencil
  , _renderStateBackBuffer :: !(Buffer Back)
  , _renderStateFrontBuffer :: !(Buffer Front)
}

-- TODO use phantom types for Cell (requires Data.Vector.Unboxed.Deriving to use newtype in vector)
newBufferArray :: (Cell, Cell) -> IO BackFrontBuffer
newBufferArray = replicate bufferSize

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

color8Code :: ColorIntensity -> Color -> Color8Code
color8Code intensity color =
  let code = fromIntegral $ colorToCode color
  in  Color8Code $ if intensity == Vivid then 8 + code else code

{-# NOINLINE screenBuffer #-}
screenBuffer :: IORef RenderState
screenBuffer =
  unsafePerformIO $ do
    -- We initialize to different colors so that in first render the whole console is drawn to.
    buf <- newBufferArray (initialCell, nocolorCell)
    let (back, front) = unzip buf
    newIORef (RenderState mkInitialState (Buffer back) (Buffer front))

mkInitialState :: Pencil
mkInitialState = Pencil 0 initialForeground initialBackground

-- | Modulo optimized for cases where most of the time,
--    a < b (for a mod b)
{-# INLINE fastMod #-}
fastMod :: Int -> Int -> Int
fastMod a b
  | 0 <= a && a < b = a          -- fast path
  | otherwise       = a `mod` b  -- slow path

{-# INLINE positionFromXY #-}
positionFromXY :: Int -> Int -> Int
positionFromXY x y = (y * bufferWidth + x) `fastMod` bufferSize

{-# INLINE xyFromPosition #-}
xyFromPosition :: Int -> (Int, Int)
xyFromPosition pos = (x, y)
  where
    pos' = pos `fastMod` bufferSize
    x = pos' - y * bufferWidth
    y = pos' `div` bufferWidth

-- functions that query/modify the buffer
bSetForeground :: ColorIntensity -> Color -> IO Color8Code
bSetForeground ci co = bSetRawForeground $ color8Code ci co

bSetRawForeground :: Color8Code -> IO Color8Code
bSetRawForeground fg = do
  (RenderState (Pencil idx prevFg prevBg) b f ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx fg prevBg) b f
  return prevFg

bSetBackground :: ColorIntensity -> Color -> IO Color8Code
bSetBackground ci co = bSetRawBackground $ color8Code ci co

bSetRawBackground :: Color8Code -> IO Color8Code
bSetRawBackground bg = do
  (RenderState (Pencil idx prevFg prevBg) b f ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx prevFg bg) b f
  return prevBg

bSetColors :: (Color8Code, Color8Code) -> IO (Color8Code, Color8Code)
bSetColors (fg,bg) = do
  (RenderState (Pencil idx prevFg prevBg) b f ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx fg bg) b f
  return (prevFg, prevBg)


bGotoXY :: Int -> Int -> IO ()
bGotoXY x y = do
  (RenderState (Pencil _ fg bg) b f) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil (positionFromXY x y) fg bg) b f

-- | Write a char and return the position of the written char in the buffer
bPutCharRaw :: Char -> IO ()
bPutCharRaw c = do
  (RenderState (Pencil idx fg bg) back _) <- readIORef screenBuffer
  writeToBack back idx $ mkCell fg bg c

bPutChars :: Int -> Char -> IO ()
bPutChars count c = do
  (RenderState (Pencil idx fg bg) b _) <- readIORef screenBuffer
  let cell = mkCell fg bg c
  mapM_ (\i -> let pos = (idx + i) `fastMod` bufferSize
               in writeToBack b pos cell) [0..pred count]

-- | Write a char and advance in the buffer
bPutChar :: Char -> IO ()
bPutChar c = do
  bPutCharRaw c
  (RenderState (Pencil idx fg bg) b f) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil (succ idx) fg bg) b f

bPutStr :: String -> IO ()
bPutStr = mapM_ bPutChar

bPutText :: Text -> IO ()
bPutText text =
  mapM_ bPutChar (unpack text)

{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Int -> Cell -> IO ()
writeToBack (Buffer b) = write b

bClear :: IO ()
bClear = do
  (RenderState _ back _) <- readIORef screenBuffer
  fillBackBuffer back initialCell

fillBackBuffer :: Buffer Back -> Cell -> IO ()
fillBackBuffer (Buffer b) cell =
  mapM_ (\pos -> write b pos cell) [0..bufferMaxIdx]

-- blit the backbuffer into the main buffer and change the screen
blitBuffer :: Bool
           -- ^ Clear the source buffer
           -> IO ()
blitBuffer clearSource = do
  (RenderState _ back front) <- readIORef screenBuffer
  drawDelta back front bufferMaxIdx clearSource Nothing

drawDelta :: Buffer Back
          -> Buffer Front
          -> Int
          -- ^ the position in the buffer
          -> Bool
          -- ^ should we clear the buffer containing the new values?
          -> Maybe Colors
          -- ^ The current console color, if it is known
          -> IO ()
drawDelta (Buffer backBuf) (Buffer frontBuf) idx clearBack mayCurrentConsoleColor
  | idx >= 0 = do
      valueToDisplay <- read backBuf idx
      valueCurrentlyDisplayed <- read frontBuf idx
      mayNewConsoleColor <-
        if valueToDisplay == valueCurrentlyDisplayed
          then
            return Nothing
          else do
            -- draw on screen
            res <- drawCell idx valueToDisplay mayCurrentConsoleColor
            -- update front buffer with drawn value
            write frontBuf idx valueToDisplay
            return $ Just res
      when (clearBack && (initialCell /= valueToDisplay)) $ write backBuf idx initialCell
      drawDelta (Buffer backBuf) (Buffer frontBuf) (pred idx) clearBack (mayNewConsoleColor <|> mayCurrentConsoleColor)
  | otherwise = return ()

drawCell :: Int -> Cell -> Maybe Colors -> IO Colors
drawCell idx cell maybeCurrentConsoleColor = do
  let (fg, bg, char) = expand cell
      (fgChange, bgChange) = maybe (True, True) (\(fg',bg') -> (fg'/=fg, bg'/=bg)) maybeCurrentConsoleColor
      sgrs = [SetPaletteColor Foreground fg | fgChange] ++
             [SetPaletteColor Background bg | bgChange]
  -- move to drawing position
  setCursorPositionFromBufferIdx idx
  if fgChange || bgChange
    then
      Prelude.putStr $ setSGRCode sgrs ++ [char]
    else
      Prelude.putChar char
  return (fg, bg)

{-# INLINE setCursorPositionFromBufferIdx #-}
setCursorPositionFromBufferIdx :: Int -> IO ()
setCursorPositionFromBufferIdx pos = do
  let (x, y) = xyFromPosition pos
  setCursorPosition y x
