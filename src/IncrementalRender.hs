-- source: https://gist.github.com/ibraimgm/40e307d70feeb4f117cd
-- with modifications:
--   use strict fields in record to avoid lazy evaluation
--   simplify bPutStr
--   make imports explicit
-- TODO allow using Text

module IncrementalRender
       (
         bSetForeground
       , bSetBackground
       , bGotoXY
       , bPutChar
       , bPutStr
       , blitBuffer
       ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)
import Data.Array.IO (IOArray, newArray, writeArray, readArray)
import System.Console.ANSI(ColorIntensity(..), Color(..), setCursorPosition, setSGR, SGR(..), ConsoleLayer(..))

-- constant data
bufferWidth :: Int
bufferWidth = 80

bufferHeight :: Int
bufferHeight = 25

bufferMax :: Int
bufferMax = bufferWidth * bufferHeight - 1 --From (0,0) to  (79, 24)

-- type definitions and global instance
type ColorPair = (ColorIntensity, Color)
type BufferCell = (ColorPair, ColorPair, Char)
type BufferArray = IOArray Int BufferCell

data ConsoleBuffer = ConsoleBuffer { currX :: !Int
                                   , currY :: !Int
                                   , currFg :: !ColorPair
                                   , currBg :: !ColorPair
                                   , currBuffer :: !BufferArray
                                   , backBuffer :: !BufferArray
                                   }

emptyBufferArray :: IO BufferArray
emptyBufferArray = newArray (0, bufferMax) (foreground, background, ' ')
  where
    foreground = (Dull, White)
    background = (Dull, Black)

{-# NOINLINE screenBuffer #-}
screenBuffer :: IORef ConsoleBuffer
screenBuffer = unsafePerformIO $ do b1 <- emptyBufferArray
                                    b2 <- emptyBufferArray
                                    newIORef (ConsoleBuffer 0 0 (Dull, White) (Dull, Black) b1 b2)

-- aux. functions
needDrawing :: BufferCell -> BufferCell -> Bool
needDrawing a b = a /= b

positionFromXY :: Int -> Int -> Int
positionFromXY x y = (y * bufferWidth + x) `mod` (bufferMax + 1)

xyFromPosition :: Int -> (Int, Int)
xyFromPosition pos = (x, y)
  where
    maxX = bufferWidth - 1
    pos' = pos `mod` (bufferMax + 1)
    x = if pos' > maxX then pos' `mod` bufferWidth else pos'
    y = if pos' >= bufferWidth then pos' `div` bufferWidth else 0

-- functions that query/modify the buffer
bSetForeground :: ColorPair -> IO ()
bSetForeground fg = do
  screen <- readIORef screenBuffer
  writeIORef screenBuffer screen{currFg = fg}

bSetBackground :: ColorPair -> IO ()
bSetBackground bg = do
  screen <- readIORef screenBuffer
  writeIORef screenBuffer screen{currBg = bg}

bGotoXY :: Int -> Int -> IO ()
bGotoXY x y = do
  screen <- readIORef screenBuffer
  writeIORef screenBuffer screen{currX = x, currY = y}

bPutChar :: Char -> IO ()
bPutChar c = do
  screen <- readIORef screenBuffer
  let x = currX screen
      y = currY screen
      fg = currFg screen
      bg = currBg screen
      pos = positionFromXY x y
      (x', y') = xyFromPosition (pos + 1)
      buff = backBuffer screen
  writeIORef screenBuffer screen{currX = x', currY = y'}
  writeArray buff pos (fg, bg, c)

bPutStr :: String -> IO ()
bPutStr = mapM_ bPutChar

-- blit the backbuffer into the main buffer and change the screen
blitBuffer :: IO ()
blitBuffer = do
  screen <- readIORef screenBuffer
  let current = currBuffer screen
      back = backBuffer screen
  applyBuffer back current bufferMax
  -- just to remove the cursor from view. Also, leave the console in a better position
  -- when the user leaves the game/simulation
  setCursorPosition (bufferHeight - 1) (bufferWidth - 1)

applyBuffer :: BufferArray -> BufferArray -> Int -> IO ()
applyBuffer _ _ (-1) = return ()
applyBuffer from to position = do
  cellFrom <- readArray from position
  cellTo <- readArray to position
  when (needDrawing cellFrom cellTo) $ drawCellChanges cellFrom cellTo position
  writeArray to position cellFrom
  applyBuffer from to (position - 1)

{-# ANN drawCellChanges "HLint: ignore Too strict if" #-}
drawCellChanges :: BufferCell -> BufferCell -> Int -> IO ()
drawCellChanges ((fromFGI, fromFGC), (fromBGI, fromBGC), fromV) ((toFGI, toFGC), (toBGI, toBGC), toV) position = do
  screen <- readIORef screenBuffer
  let (bbFGI, bbFGC) = currFg screen
      (bbBGI, bbBGC) = currBg screen
      colorChanged = (fromFGI /= toFGI) || (fromFGC /= toFGC) || (fromBGI /= toBGI) || (fromBGC /= toBGC) ||
                     (fromFGI /= bbFGI) || (fromFGC /= bbFGC) || (fromBGI /= bbBGI) || (fromBGC /= bbBGC)
      valueChanged = fromV /= toV
  if colorChanged
    then setSGR [SetColor Foreground fromFGI fromFGC, SetColor Background fromBGI fromBGC]
    else setSGR [SetColor Foreground bbFGI bbFGC, SetColor Background bbBGI bbBGC]
  when (valueChanged || colorChanged) $ do gotoCursorPosition position
                                           putChar fromV

gotoCursorPosition :: Int -> IO ()
gotoCursorPosition pos = setCursorPosition y x
  where
    (x, y) = xyFromPosition pos
