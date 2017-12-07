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
--   - Add support for 8-bits ANSI colors

module Render.Backends.Internal.Delta
       (
         bSetForeground
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
       , Color8Code(..) -- Constructors is exported but the preferred way to describe colors is with
                        -- (ColorIntensity, Color) or RGB8Color or Gray8Color
       ) where

import           Imajuscule.Prelude

import qualified Prelude ( putChar, putStr )

import           Control.Monad( when )

import           Data.Array.IO( IOArray
                              , newArray
                              , writeArray
                              , readArray )
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

-- TODO use an IOUArray that unboxes elements, and a representation with an Int:
-- using bit shifts, we can store at least 2 Color8Code and a char in an Int.
type BufferArray = IOArray Int BufferCell

data ConsoleBuffer = ConsoleBuffer { currX :: !Int
                                   , currY :: !Int
                                   , currFg :: !Color8Code
                                   , currBg :: !Color8Code
                                   , currBuffer :: !BufferArray
                                   , backBuffer :: !BufferArray
                                   }

newBufferArray :: BufferCell -> IO BufferArray
newBufferArray = newArray (0, bufferMaxIdx)

initialForeground :: Color8Code
initialForeground = xterm256ColorToCode $ RGBColor $ RGB 5 5 5

initialBackground :: Color8Code
initialBackground = xterm256ColorToCode $ RGBColor $ RGB 0 0 0

noColor :: Color8Code
noColor = Color8Code (-1)

initialCell :: BufferCell
initialCell = mkBufferCell initialForeground initialBackground ' '

nocolorCell :: BufferCell
nocolorCell = mkBufferCell noColor noColor ' '

color8Code :: ColorIntensity -> Color -> Color8Code
color8Code intensity color =
  let code = fromIntegral $ colorToCode color
  in  Color8Code $ if intensity == Vivid then 8 + code else code

{-# NOINLINE screenBuffer #-}
screenBuffer :: IORef ConsoleBuffer
screenBuffer = unsafePerformIO $ do cur  <- newBufferArray nocolorCell -- We initialize to different colors
                                    back <- newBufferArray initialCell -- so that in first render the whole console
                                                                       -- is drawn to.
                                    newIORef (ConsoleBuffer 0 0 initialForeground initialBackground cur back)

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
  screen@(ConsoleBuffer _ _ prev _ _ _ ) <- readIORef screenBuffer
  writeIORef screenBuffer screen{currFg = fg}
  return prev

bSetBackground :: ColorIntensity -> Color -> IO ()
bSetBackground ci co = bSetRawBackground $ color8Code ci co

bSetRawBackground :: Color8Code -> IO ()
bSetRawBackground bg = do
  screen <- readIORef screenBuffer
  writeIORef screenBuffer screen{currBg = bg}

bSetColors :: (Color8Code, Color8Code) -> IO (Color8Code, Color8Code)
bSetColors (fg,bg) = do
  screen@(ConsoleBuffer _ _ prevFg prevBg _ _ ) <- readIORef screenBuffer
  writeIORef screenBuffer screen{currFg = fg, currBg = bg}
  return (prevFg, prevBg)


bGotoXY :: Int -> Int -> IO ()
bGotoXY x y = do
  screen <- readIORef screenBuffer
  writeIORef screenBuffer screen{currX = x, currY = y}

-- | Write a char and return the position of the written char in the buffer
bPutCharRaw :: Char -> IO Int
bPutCharRaw c = do
  (ConsoleBuffer x y fg bg _ buff) <- readIORef screenBuffer
  let pos = positionFromXY x y
  writeArray buff pos $ mkBufferCell fg bg c
  return pos

bPutChars :: Int -> Char -> IO ()
bPutChars count c = do
  (ConsoleBuffer x y fg bg _ buff) <- readIORef screenBuffer
  let cell = mkBufferCell fg bg c
  mapM_ (\i -> let pos = positionFromXY (x+i) y
               in writeArray buff pos cell) [0..count-1]

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
  fillBuffer initialCell buff

fillBuffer :: BufferCell -> BufferArray -> IO ()
fillBuffer cell buffer = mapM_ (\pos -> writeArray buffer pos cell) [0..bufferMaxIdx]

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
      mayNewConsoleColor <- if cellFrom == cellTo
                              then
                                -- no need to draw
                                return Nothing
                              else do
                                gotoCursorPosition position
                                res <- drawCell cellFrom mayCurrentConsoleColor
                                return $ Just res
      writeArray to position cellFrom
      applyBuffer from to (pred position) clearFrom (mayNewConsoleColor <|> mayCurrentConsoleColor)
  | otherwise = return ()

drawCell :: BufferCell -> Maybe Colors -> IO Colors
drawCell cell maybeCurrentConsoleColor = do
  let (fg, bg, char) = expand cell
      (fgChange, bgChange) = maybe (True, True) (\(fg',bg') -> (fg'/=fg, bg'/=bg)) maybeCurrentConsoleColor
      sgrs = [SetPaletteColor Foreground fg | fgChange] ++
             [SetPaletteColor Background bg | bgChange]
  if null sgrs
    then
      Prelude.putChar char
    else
      Prelude.putStr $ setSGRCode sgrs ++ [char]
  return (fg, bg)

gotoCursorPosition :: Int -> IO ()
gotoCursorPosition pos = setCursorPosition y x
  where
    (x, y) = xyFromPosition pos
