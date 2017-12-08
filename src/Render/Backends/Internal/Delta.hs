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

{--import           Data.Array.IO( IOUArray
                              , newArray
                              , writeArray
                              , readArray )
--}
import Data.Vector.Unboxed.Mutable( IOVector, replicate, read, write, unzip )

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

type InterleavedBackFrontBuffer = IOVector (Cell, Cell)

data Pencil = Pencil {
    _pencilBufferIndex :: !Int
  , _pencilForeground :: !Color8Code
  , _pencilBackground :: !Color8Code
}

data RenderState = RenderState {
    _renderStatePencil :: !Pencil
  , _renderStateBuffer :: !InterleavedBackFrontBuffer
}

-- TODO use phantom types
newBufferArray :: (Cell, Cell) -> IO InterleavedBackFrontBuffer
newBufferArray = Data.Vector.Unboxed.Mutable.replicate bufferSize

initialForeground :: Color8Code
initialForeground = xterm256ColorToCode $ RGBColor $ RGB 5 5 5

initialBackground :: Color8Code
initialBackground = xterm256ColorToCode $ RGBColor $ RGB 0 0 0

noColor :: Color8Code
noColor = Color8Code (-1)

initialCell :: Cell
initialCell = mkCell initialForeground initialBackground ' '

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
    newIORef (RenderState mkInitialState buf)

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
  (RenderState (Pencil idx prevFg prevBg) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx fg prevBg) b
  return prevFg

bSetBackground :: ColorIntensity -> Color -> IO Color8Code
bSetBackground ci co = bSetRawBackground $ color8Code ci co

bSetRawBackground :: Color8Code -> IO Color8Code
bSetRawBackground bg = do
  (RenderState (Pencil idx prevFg prevBg) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx prevFg bg) b
  return prevBg

bSetColors :: (Color8Code, Color8Code) -> IO (Color8Code, Color8Code)
bSetColors (fg,bg) = do
  (RenderState (Pencil idx prevFg prevBg) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx fg bg) b
  return (prevFg, prevBg)


bGotoXY :: Int -> Int -> IO ()
bGotoXY x y = do
  (RenderState (Pencil _ fg bg) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil (positionFromXY x y) fg bg) b

-- | Write a char and return the position of the written char in the buffer
bPutCharRaw :: Char -> IO ()
bPutCharRaw c = do
  (RenderState (Pencil idx fg bg) buff) <- readIORef screenBuffer
  writeToBack buff idx $ mkCell fg bg c

bPutChars :: Int -> Char -> IO ()
bPutChars count c = do
  (RenderState (Pencil idx fg bg) buff) <- readIORef screenBuffer
  let cell = mkCell fg bg c
  mapM_ (\i -> let pos = (idx + i) `fastMod` bufferSize
               in writeToBack buff pos cell) [0..pred count]

-- | Write a char and advance in the buffer
bPutChar :: Char -> IO ()
bPutChar c = do
  bPutCharRaw c
  (RenderState (Pencil idx fg bg) buff) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil (succ idx) fg bg) buff

bPutStr :: String -> IO ()
bPutStr = mapM_ bPutChar

bPutText :: Text -> IO ()
bPutText text =
  mapM_ bPutChar (unpack text)

writeToBack :: InterleavedBackFrontBuffer -> Int -> Cell -> IO ()
writeToBack buff =
  write (fst $ unzip buff)

bClear :: IO ()
bClear = do
  (RenderState _ buff) <- readIORef screenBuffer
  fillBackBuffer initialCell buff

fillBackBuffer :: Cell -> InterleavedBackFrontBuffer -> IO ()
fillBackBuffer cell buffer =
  mapM_ (\pos -> writeToBack buffer pos cell) [0..bufferMaxIdx]

-- blit the backbuffer into the main buffer and change the screen
blitBuffer :: Bool
           -- ^ Clear the source buffer
           -> IO ()
blitBuffer clearSource = do
  (RenderState _ buff) <- readIORef screenBuffer
  drawDelta buff bufferMaxIdx clearSource Nothing

-- TODO simplify the logic by decoupling front and back buffer updates
drawDelta :: InterleavedBackFrontBuffer
          -- ^ buffer containing new and old values
          -> Int
          -- ^ the position in the buffer
          -> Bool
          -- ^ should we clear the buffer containing the new values?
          -> Maybe Colors
          -- ^ The current console color, if it is known
          -> IO ()
drawDelta buffer position clearBack mayCurrentConsoleColor
  | position >= 0 = do
      (back, front) <- read buffer position
      mayNewConsoleColor <-
        if back == front
          then
            -- no need to draw
            return Nothing
          else do
            gotoCursorPosition position
            res <- drawCell back mayCurrentConsoleColor
            return $ Just res
      let newBack =
            if clearBack
              then initialCell
              else back
      write buffer position (newBack, back)
      drawDelta buffer (pred position) clearBack (mayNewConsoleColor <|> mayCurrentConsoleColor)
  | otherwise = return ()

drawCell :: Cell -> Maybe Colors -> IO Colors
drawCell cell maybeCurrentConsoleColor = do
  let (fg, bg, char) = expand cell
      (fgChange, bgChange) = maybe (True, True) (\(fg',bg') -> (fg'/=fg, bg'/=bg)) maybeCurrentConsoleColor
      sgrs = [SetPaletteColor Foreground fg | fgChange] ++
             [SetPaletteColor Background bg | bgChange]
  if fgChange || bgChange
    then
      Prelude.putStr $ setSGRCode sgrs ++ [char]
    else
      Prelude.putChar char
  return (fg, bg)

gotoCursorPosition :: Int -> IO ()
gotoCursorPosition pos = setCursorPosition y x
  where
    (x, y) = xyFromPosition pos
