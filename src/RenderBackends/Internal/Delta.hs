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
--   - Reworked logic of drawCell / applyBuffer to draw only when strictly required.
--     To that end, we keep track of the current color of the console while drawing.
--   - Add support for Raw8Color (8-bits ANSI colors)

module RenderBackends.Internal.Delta
       (
         bSetForeground
       , bSetRawForeground
       , bSetBackground
       , bSetRawBackground
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
       , RGB8Color(..)
       , Gray8Color(..)
       , Raw8Color(..) -- Constructors is exported bu the preferred way to describe colors is with
                       -- (ColorIntensity, Color) or RGB8Color or Gray8Color
       ) where

import           Imajuscule.Prelude

import qualified Prelude ( putChar, putStr )

import           Data.IORef( IORef
                           , newIORef
                           , readIORef
                           , writeIORef )
import           Data.String( String )
import           Data.Text( Text, unpack )

import           System.IO.Unsafe( unsafePerformIO )
import           Control.Monad( when )
import           Data.Array.IO( IOArray
                              , newArray
                              , writeArray
                              , readArray )
import           System.Console.ANSI( ColorIntensity(..)
                                    , Color(..)
                                    , Raw8Color(..)
                                    , RGB8Color(..)
                                    , Gray8Color(..)
                                    , setCursorPosition
                                    , setSGRCode
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

-- We could use a newtype but I'm worried that unboxing doesn't happen recursively enough
-- to unbox everything here...
type Colors = (Raw8Color, Raw8Color) -- (foregroud color, background color)

type BufferCell = (Colors, Char)
type BufferArray = IOArray Int BufferCell

data ConsoleBuffer = ConsoleBuffer { currX :: !Int
                                   , currY :: !Int
                                   , currFg :: !Raw8Color
                                   , currBg :: !Raw8Color
                                   , currBuffer :: !BufferArray
                                   , backBuffer :: !BufferArray
                                   }

emptyBufferArray :: IO BufferArray
emptyBufferArray = newArray (0, bufferMaxIdx) initialCell

initialForeground :: Raw8Color
initialForeground = color8Code Dull White

initialBackground :: Raw8Color
initialBackground = color8Code Dull Black

initialCell :: BufferCell
initialCell = ((initialForeground, initialBackground), ' ')

color8Code :: ColorIntensity -> Color -> Raw8Color
color8Code intensity color =
  let code = colorToCode color
  in  Raw8Color $ if intensity == Vivid then 8 + code else code

-- copied from System.Control.ANSI
colorToCode :: Color -> Int
colorToCode color = case color of
  Black   -> 0
  Red     -> 1
  Green   -> 2
  Yellow  -> 3
  Blue    -> 4
  Magenta -> 5
  Cyan    -> 6
  White   -> 7

{-# NOINLINE screenBuffer #-}
screenBuffer :: IORef ConsoleBuffer
screenBuffer = unsafePerformIO $ do b1 <- emptyBufferArray
                                    b2 <- emptyBufferArray
                                    newIORef (ConsoleBuffer 0 0 initialForeground initialBackground b1 b2)

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
bSetForeground :: ColorPair -> IO Raw8Color
bSetForeground p = bSetRawForeground (uncurry color8Code p)

bSetRawForeground :: Raw8Color -> IO Raw8Color
bSetRawForeground fg = do
  screen@(ConsoleBuffer _ _ prev _ _ _ ) <- readIORef screenBuffer
  writeIORef screenBuffer screen{currFg = fg}
  return prev

bSetBackground :: ColorPair -> IO ()
bSetBackground p = bSetRawBackground (uncurry color8Code p)

bSetRawBackground :: Raw8Color -> IO ()
bSetRawBackground bg = do
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
  writeArray buff pos ((fg, bg), c)
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
  applyBuffer back current bufferMaxIdx clearSource Nothing

applyBuffer :: BufferArray
            -- ^ buffer containing new values
            -> BufferArray
            -- ^ buffer containing old values
            -> Int
            -- ^ the position in the buffers
            -> Bool
            -- ^ should we clear the buffer containing the new values?
            -> Maybe Colors
            -- ^ The current console color, if it is known
            -> IO ()
applyBuffer from to position clearFrom mayCurrentConsoleColor
  | position >= 0 = do
      cellFrom <- readArray from position
      when clearFrom $ writeArray from position initialCell
      cellTo <- readArray to position
      mayNewConsoleColor <- if needDrawing cellFrom cellTo
                              then
                                do
                                  gotoCursorPosition position
                                  res <- drawCell cellFrom mayCurrentConsoleColor
                                  return $ Just res
                              else
                                return Nothing
      writeArray to position cellFrom
      applyBuffer from to (pred position) clearFrom (mayNewConsoleColor <|> mayCurrentConsoleColor)
  | otherwise = return ()

drawCell :: BufferCell -> Maybe Colors -> IO Colors
drawCell (color@(fg, bg), char) maybeCurrentConsoleColor = do
  let (fgChange, bgChange) = maybe (True, True) (\(fg',bg') -> (fg'/=fg, bg'/=bg)) maybeCurrentConsoleColor
      sgrs = [SetRaw8Color Foreground fg | fgChange] ++
             [SetRaw8Color Background bg | bgChange]
  if null sgrs
    then
      Prelude.putChar char
    else
      Prelude.putStr $ setSGRCode sgrs ++ [char]
  return color

gotoCursorPosition :: Int -> IO ()
gotoCursorPosition pos = setCursorPosition y x
  where
    (x, y) = xyFromPosition pos
