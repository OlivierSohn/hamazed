{-# LANGUAGE NoImplicitPrelude #-}

{-| This module is designed to render games / animations in the console
efficiently, without [screen tearing effect](https://en.wikipedia.org/wiki/Screen_tearing).

The [screen tearing effect](https://en.wikipedia.org/wiki/Screen_tearing)
can occur with naïve rendering techniques where we
would render to the console "as we draw", thereby sending "a lot of data"
(TODO quantify) to the stdout buffer.
The stdout buffer having a limited buffer size, it will do intermediate flushes
while rendering a single frame, so we will see partial frames rendered in the
console.

TODO explain that we can configure stdout buffering mode to block buffering and experiment
to see to what extent this fixes the problem in the naive case (is there an upper bound
to the stdout buffer capacity?)

TODO should this module handle cursor hiding?
TODO should this module expose more draw functions, to render ColorString for
example?

Here we use a double-buffering technique:

* A /front/ buffer contains the content of the console.
* A /back/ buffer contains the frame to be rendered.

The difference between front and back buffers tells us which parts of the
screen have changed in the frame to be rendered, so we don't need to send rendering
commands for the entire frame, we just send commands for the parts of the console
that actually changed. We also compute
the optimal order in which to render the changed locations so as to minimize the
number of data to send (grouping by colors for example to minimize the number of
color change commands sent).

Hence, the amount of data sent to stdout buffer is drastically
reduced, thereby reducing the probability of intermediate flushes.

Typically for each frame, clients of this module will:

1. Call "draw" functions to modify the content of the back buffer.

    * It is very fast compared to rendering in the console directly, so it is
    perfectly okay to draw several times at the same location.

2. Call 'renderFrame'.

The functions of this module are in the IO monad because a top level
IORef is used to store states : current colors, current draw position,
front and back buffers, and internal buffers.
-}
module Render.Backends.Internal.Delta
       (
       -- * Setup
         setCanvasDimensions
       -- * Fill the back buffer
       , fill
       -- * Draw to the back buffer
       -- ** Set states for subsequent 'draw***' calls
       -- *** Location
       , setDrawLocation
       -- *** Colors
       , setDrawColor
       , setDrawColors
       , restoreDrawColors
       -- ** Draw using states
       , drawChar
       , drawChars
       , drawStr
       , drawTxt
       -- * Render to the console
       , renderFrame
       -- * Reexports
       , module Render.Backends.Internal.Types
       ) where


import           Imajuscule.Prelude hiding (replicate)

import qualified Prelude ( putChar, putStr )

import           Data.Vector.Algorithms.Intro
import           Data.Vector.Unboxed.Mutable( IOVector, replicate, read, write, unzip )
import qualified Render.Backends.Internal.UnboxedDynamic as Dyn ( IOVector
                                                                , accessUnderlying, length, new, read, clear, pushBack )
import           Data.Colour.SRGB( RGB(..) )
import           Data.IORef( IORef , newIORef , readIORef , writeIORef )
import           Data.Maybe( fromMaybe )
import           Data.String( String )
import           Data.Text( Text, unpack )
import           Data.Word( Word32, Word16 )

import           System.Console.ANSI( xterm256ColorToCode, Color8Code(..), Xterm256Color(..)
                                    , setCursorPosition, setSGRCode, SGR(..), ConsoleLayer(..) )
import           System.IO( stdout, hFlush )
import           System.IO.Unsafe( unsafePerformIO )

import           Render.Backends.Internal.BufferCell
import           Render.Backends.Internal.Types


type BackFrontBuffer = IOVector (Cell, Cell)

newtype Buffer a = Buffer (IOVector Cell)

newtype Delta = Delta (Dyn.IOVector Cell)

-- | Buffer types
data Back
data Front

data Pencil = Pencil {
    _pencilBufferIndex :: {-# UNPACK #-} !Word16
  , _pencilColors :: {-# UNPACK #-} !Colors
}

data Buffers = Buffers {
    _renderStateBackBuffer :: !(Buffer Back)
  , _renderStateFrontBuffer :: !(Buffer Front)
  , _buffersSize :: !Word16
  , _buffersWidth :: !Word16
  , _buffersHeight :: !Word16
  , _buffersDelta :: !Delta
  -- ^ buffer used in renderFrame
}


-- | Resizes the canvas, if needed
--  (this function does nothing if it was previously called with the same arguments).
setCanvasDimensions :: Word16
                    -- ^ Width
                    -> Word16
                    -- ^ Height
                    -> IO ()
setCanvasDimensions width height = do
  (RenderState (Pencil idx colors) (Buffers _ _ _ prevWidth prevHeight _)) <- readIORef screenBuffer
  if prevWidth == width && prevHeight == height
    then
      return ()
    else do
      buffers@(Buffers _ _ size _ _ _) <- mkBuffers width height
      let newPencilIdx = idx `fastMod` size
      writeIORef screenBuffer $ RenderState (Pencil newPencilIdx colors) buffers


-- | Creates buffers for given width and height, replaces 0 width or height by 1.
mkBuffers :: Word16 -> Word16 -> IO Buffers
mkBuffers width' height' = do
  let width  = max 1 width'
      height = max 1 height'
      sz = fromIntegral width * fromIntegral height :: Word32
      sz' = fromIntegral sz
  -- indexed cells use a Word16 index so size cannot be too big
  when (sz > fromIntegral (maxBound :: Word16)) $
    error $ "buffer size cannot be bigger than " ++ show (maxBound :: Word16) ++
            " : " ++ show (sz, width, height)
  -- We initialize to different colors so that in first render the whole console is drawn to.
  buf <- newBufferArray sz' (initialCell, nocolorCell)
  delta <- Dyn.new $ fromIntegral sz -- reserve the maximum possible size
  let (back, front) = unzip buf
  return $ Buffers (Buffer back) (Buffer front) sz' width height (Delta delta)

data RenderState = RenderState {
    _renderStatePencil :: !Pencil
  , _renderStateBuffers :: !Buffers
}


-- TODO use phantom types for Cell (requires Data.Vector.Unboxed.Deriving to use newtype in vector)
newBufferArray :: Word16 -> (Cell, Cell) -> IO BackFrontBuffer
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
  let res = 0x10E7000000000020 -- optimization for 'renderFrame' (TODO check if the compiler would have found it)
  in assert (mkCell initialColors ' ' == res) res


{-# INLINE initialColors #-}
initialColors :: Colors
initialColors = Colors initialBackground initialForeground


{-# INLINE noColors #-}
noColors :: Colors
noColors = Colors noColor noColor


nocolorCell :: Cell
nocolorCell = mkCell noColors ' '


{-# NOINLINE screenBuffer #-}
screenBuffer :: IORef RenderState
screenBuffer =
  unsafePerformIO $ do
    buffers <- mkBuffers 0 0
    newIORef (RenderState mkInitialState buffers)


mkInitialState :: Pencil
mkInitialState = Pencil 0 initialColors


-- | Modulo optimized for cases where most of the time,
--    a < b (for a mod b)
{-# INLINE fastMod #-}
fastMod :: Word16 -> Word16 -> Word16
fastMod a b
  | 0 <= a && a < b = a          -- fast path
  | otherwise       = a `mod` b  -- slow path


{-# INLINE indexFromXY #-}
indexFromXY :: Buffers -> Word16 -> Word16 -> Word16
indexFromXY (Buffers _ _ size width _ _) x y =
  (y * width + x) `fastMod` size


{-# INLINE xyFromIndex #-}
xyFromIndex :: Buffers -> Word16 -> (Word16, Word16)
xyFromIndex (Buffers _ _ size width _ _) idx =
    (x, y)
  where
    pos' = idx `fastMod` size
    x = pos' - y * width
    y = pos' `div` width


setDrawColor :: ConsoleLayer -> Color8Code -> IO Colors
setDrawColor layer color = do
  (RenderState (Pencil idx prevColors@(Colors prevBg prevFg)) b ) <- readIORef screenBuffer
  let newColors = case layer of
        Foreground -> Colors prevBg color
        Background -> Colors color prevFg
  writeIORef screenBuffer $ RenderState (Pencil idx newColors) b
  return prevColors


setDrawColors :: Colors -> IO Colors
setDrawColors colors = do
  (RenderState (Pencil idx prevColors) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx colors) b
  return prevColors


restoreDrawColors :: Colors -> IO ()
restoreDrawColors = void . setDrawColors


setDrawLocation :: Int -> Int -> IO ()
setDrawLocation x y
  | x < 0 || y < 0 = error $ "cannot draw to negative location " ++ show (x,y)
  | otherwise      = setDrawLocation' (fromIntegral x) (fromIntegral y)


setDrawLocation' :: Word16 -> Word16 -> IO ()
setDrawLocation' x y = do
  (RenderState (Pencil _ colors) b) <- readIORef screenBuffer
  let idx = indexFromXY b x y
  writeIORef screenBuffer $ RenderState (Pencil idx colors) b


-- | Draws a 'Char' at the current draw location, with the current draw colors.
drawChar :: Char
         -> IO ()
drawChar c = do
  (RenderState (Pencil idx colors) (Buffers back _ _ _ _ _)) <- readIORef screenBuffer
  putCharRawAt back idx colors c


putCharRawAt :: Buffer Back -> Word16 -> Colors -> Char -> IO ()
putCharRawAt back idx colors c =
  writeToBack back idx $ mkCell colors c


-- | Draws multiple times the same 'Char' starting from the current draw location,
--   using current draw colors.
drawChars :: Int
          -- ^ Number of repetitions.
          -> Char
          -> IO ()
drawChars count c = do
  (RenderState (Pencil idx colors) (Buffers back _ size _ _ _)) <- readIORef screenBuffer
  let cell = mkCell colors c
  mapM_ (\i -> let pos = (idx + fromIntegral i) `fastMod` size
               in writeToBack back pos cell) [0..pred count]


-- | Draws a 'String' starting from the current draw location,
--   using current draw colors.
drawStr :: String -> IO ()
drawStr str = do
  (RenderState (Pencil idx colors) (Buffers back _ size _ _ _)) <- readIORef screenBuffer
  mapM_ (\(c, i) -> putCharRawAt back (idx+i `fastMod` size) colors c) $ zip str [0..]


-- | Draws a 'Text' starting from the current draw location,
--   using current draw colors.
drawTxt :: Text -> IO ()
drawTxt text = drawStr $ unpack text


{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Word16 -> Cell -> IO ()
writeToBack (Buffer b) pos = write b (fromIntegral pos)


-- | Fills the entire canvas with colors and a char.
fill :: Maybe Colors
     -- ^ When Nothing, the initial colors are used.
     -> Maybe Char
     -- ^ When Nothing, a space character is used.
     -> IO ()
fill mayColors mayChar = do
  let colors = fromMaybe initialColors mayColors
      char = fromMaybe ' ' mayChar
  _ <- setDrawColors colors
  (RenderState _ b) <- readIORef screenBuffer
  fillBackBuffer b colors char


fillBackBuffer :: Buffers -> Colors -> Char -> IO ()
fillBackBuffer (Buffers (Buffer b) _ size _ _ _) colors char = do
  let cell = mkCell colors char
  mapM_ (\pos -> write b pos cell) [0..fromIntegral $ pred size]


{- |
* Send rendering commands to the stdout buffer (just for the selected locations where
      back and front buffers differ), then flush stdout.
* Copy the back buffer to the front buffer
* Reset (clear) the content of the back buffer
-}
renderFrame :: Bool
              -- ^ If True, after submission the drawing is reset to the initial color.
              -- This can also be achieved by calling "fill Nothing Nothing" after this call,
              -- at the cost of one more traversal of the back buffer.
              -> IO ()
renderFrame clearBack = do
  (RenderState _ buffers@(Buffers _ _ _ _ _ (Delta delta))) <- readIORef screenBuffer
  computeDelta buffers 0 clearBack
  szDelta <- Dyn.length delta
  underlying <- Dyn.accessUnderlying delta
  sort underlying -- this will group by colors first (background then foreground), then position
  -- We ignore this color value. We could store it and use it to initiate the recursion
  -- at next render but if the client renders with another library inbetweeen, this value
  -- would be wrong, so we can ignore it here for more robustness.
  _ <- renderDelta szDelta 0 Nothing Nothing buffers
  Dyn.clear delta
  hFlush stdout

renderDelta :: Int
            -> Int
            -> Maybe Colors
            -> Maybe Word16
            -> Buffers
            -> IO Colors
renderDelta size index prevColors prevIndex
  b@(Buffers _ _ _ _ _ (Delta delta))
 | index == size = return $ Colors (Color8Code 0) (Color8Code 0)
 | otherwise = do
    c <- Dyn.read delta index
    let (bg, fg, idx, char) = expandIndexed c
        prevRendered = maybe False (== pred idx) prevIndex
    setCursorPositionIfNeeded b idx prevRendered
    usedColor <- drawCell bg fg char prevColors
    renderDelta size (succ index) (Just usedColor) (Just idx) b


computeDelta :: Buffers
             -> Word16
             -- ^ the buffer index
             -> Bool
             -- ^ should we clear the buffer containing the new values?
             -> IO ()
computeDelta
 b@(Buffers (Buffer backBuf) (Buffer frontBuf) size _ _ (Delta delta))
 idx
 clearBack
  | idx == size = return ()
  | otherwise = do
      let i = fromIntegral idx
      valueToDisplay <- read backBuf i
      valueCurrentlyDisplayed <- read frontBuf i
      when (valueToDisplay /= valueCurrentlyDisplayed) $ do
          -- update front buffer with drawn value
          write frontBuf i valueToDisplay
          Dyn.pushBack delta $ mkIndexedCell valueToDisplay $ fromIntegral idx
      when (clearBack && (initialCell /= valueToDisplay)) $ write backBuf i initialCell
      computeDelta b (succ idx) clearBack

-- | The command to set the cursor position to 23,45 is "\ESC[23;45H",
-- its size is 9 bytes : one order of magnitude more than the size
-- of a char, so we avoid sending this command when not strictly needed.
{-# INLINE setCursorPositionIfNeeded #-}
setCursorPositionIfNeeded :: Buffers
                          -> Word16
                          -- ^ the buffer index
                          -> Bool
                          -- ^ True if a char was rendered at the previous buffer index
                          -> IO ()
setCursorPositionIfNeeded b idx predPosRendered = do
  let (colIdx, rowIdx) = xyFromIndex b idx
      shouldSetCursorPosition =
      -- We assume that the buffer width is not equal to terminal width,
      -- so even if the previous position was rendered,
      -- the cursor may not be located at the beginning of the line.
        colIdx == 0
      -- If the previous buffer position was rendered, the cursor position has
      -- automatically advanced to the next column (or to the beginning of
      -- the next line if it was the last terminal column).
        || not predPosRendered
  when shouldSetCursorPosition $ setCursorPosition (fromIntegral rowIdx) (fromIntegral colIdx)

{-# INLINE drawCell #-}
drawCell :: Color8Code -> Color8Code -> Char -> Maybe Colors -> IO Colors
drawCell bg fg char maybeCurrentConsoleColor = do
  let (bgChange, fgChange, usedFg) =
        maybe
          (True, True, fg)
          (\(Colors bg' fg') ->
              -- use foreground color if we don't draw a space
              let useFg = char /= ' ' -- I don't use Data.Char.isSpace, it could be slower
                  usedFg' = if useFg
                             then
                               fg
                             else
                               fg'
              in (bg'/=bg, fg'/=usedFg', usedFg'))
            maybeCurrentConsoleColor
      sgrs = [SetPaletteColor Foreground fg | fgChange] ++
             [SetPaletteColor Background bg | bgChange]

  if bgChange || fgChange
    then
      Prelude.putStr $ setSGRCode sgrs ++ [char]
    else
      Prelude.putChar char
  return $ Colors bg usedFg
