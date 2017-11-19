{-# LANGUAGE NoImplicitPrelude #-}

-- Initial code from Rafael Ibraim : https://gist.github.com/ibraimgm/40e307d70feeb4f117cd
--
-- With the following modifications:
--   - use strict fields in record to avoid lazy evaluation
--   - simplify code of bPutStr
--   - write applyBuffer using guards
--   - make imports explicit
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
--   - inline some functions

module RenderBackends.Internal.Delta
       (
         bSetForeground
       , bSetBackground
       , bGotoXY
       , bPutChar
       , bPutCharRaw
       , bPutStr
       , bPutText
       , bClear
       , blitBuffer
       -- reexports from System.Console.ANSI
       , ColorIntensity(..)
       , Color(..)
       ) where

import           Imajuscule.Prelude

import qualified Prelude ( putChar )

import           Data.IORef( IORef
                           , newIORef
                           , readIORef
                           , writeIORef )
import           Data.String( String )
import           Data.Text( Text
                          , unpack )

import           System.IO.Unsafe( unsafePerformIO )
import           Control.Monad( when )
import           Data.Array.IO( IOArray
                              , newArray
                              , writeArray
                              , readArray )
import           System.Console.ANSI( ColorIntensity(..)
                                    , Color(..)
                                    , setCursorPosition
                                    , setSGR
                                    , SGR(..)
                                    , ConsoleLayer(..) )

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
emptyBufferArray = newArray (0, bufferMaxIdx) initialCell

initialCell :: BufferCell
initialCell = (foreground, background, ' ')
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

-- | Write a char and return the position of the written char in the buffer
bPutCharRaw :: Char -> IO Int
bPutCharRaw c = do
  screen <- readIORef screenBuffer
  let x = currX screen
      y = currY screen
      fg = currFg screen
      bg = currBg screen
      pos = positionFromXY x y
      buff = backBuffer screen
  writeArray buff pos (fg, bg, c)
  return pos

-- | Write a char and advance in the buffer
bPutChar :: Char -> IO ()
bPutChar c = do
  pos <- bPutCharRaw c
  screen <- readIORef screenBuffer
  let (x', y') = xyFromPosition (pos + 1)
  writeIORef screenBuffer screen{currX = x', currY = y'}

bPutStr :: String -> IO ()
bPutStr = mapM_ bPutChar

bPutText :: Text -> IO ()
bPutText text =
  mapM_ bPutChar (unpack text)

bClear :: IO ()
bClear = do
  screen <- readIORef screenBuffer
  let buff = backBuffer screen
  mapM_ (\pos -> writeArray buff pos initialCell) [0..bufferMaxIdx]

-- blit the backbuffer into the main buffer and change the screen
blitBuffer :: Bool
           -- ^ Clear the source buffer
           -> IO ()
blitBuffer clearSource = do
  screen <- readIORef screenBuffer
  let current = currBuffer screen
      back = backBuffer screen
  applyBuffer back current bufferMaxIdx clearSource

applyBuffer :: BufferArray -> BufferArray -> Int -> Bool -> IO ()
applyBuffer from to position clearFrom
  | position < 0 = return ()
  | otherwise = do
  cellFrom <- readArray from position
  when clearFrom $ writeArray from position initialCell
  cellTo <- readArray to position
  when (needDrawing cellFrom cellTo) $ drawCellChanges cellFrom cellTo position
  writeArray to position cellFrom
  applyBuffer from to (pred position) clearFrom

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
                                           Prelude.putChar fromV

gotoCursorPosition :: Int -> IO ()
gotoCursorPosition pos = setCursorPosition y x
  where
    (x, y) = xyFromPosition pos
