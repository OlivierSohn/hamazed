{-# LANGUAGE NoImplicitPrelude #-}

{-|
= Purpose
Render games or animations in the console, without
<https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.

= Design goals
* Use generic techniques to avoid
<https://en.wikipedia.org/wiki/Screen_tearing screen tearing> for all kinds of
game graphics.
* Minimize memory footprint and run-time overhead.

= Features
* <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit colors> support
* <http://www.unicode.org/ Unicode> support

= How it started

In the beginnings of
<https://github.com/OlivierSohn/hamazed my first ascii based game>,
the render loop was as naïve as:

* Clear the console.
* Render the game elements to the console directly.
* Flush stdout.

It worked well whilst a frame was ~100 different locations in a single color.

As color animations were introduced, flickering / tearing
effects started to occur on frames with more rendering locations, because stdout was saturated by
rendering commands and would flush before all commands corresponding to this frame were issued.

To fix this I maximized the size of stdout buffer by using
<https://hackage.haskell.org/package/base-4.10.1.0/docs/System-IO.html#v:hSetBuffering hSetBuffering>
with
<https://hackage.haskell.org/package/base-4.10.1.0/docs/System-IO.html#v:BlockBuffering BlockBuffering>
parameter.

(Note that to measure the actual stdout size you can use 'testStdoutSizes'),

> hSetBuffering stdout $ BlockBuffering $ Just (maxBound :: Int)

This solved the problem, only until more animations and more color changes were introduced.

To fix <https://github.com/feuerbach/ansi-terminal/issues/5 this issue>,
<https://github.com/ibraimgm Rafael Ibraim> shared
<https://gist.github.com/ibraimgm/40e307d70feeb4f117cd a solution using double buffering>
where the render loop is:

* Clear the back buffer.
* Draw the game elements to the back buffer.
* Render the difference between the front and the back buffer to the console.
* Flush stdout.
* Copy the back buffer to the front buffer.

This approach fixed the rendering of my game.

Still, I wanted to continue improving it, to cover more use-cases.
So I added the following optimizations:

* On average, one @color change@ command "costs" 20 bytes ("\ESC[48;5;167;38;5;255m").
Grouping the rendered locations by color allows to issue one command for the group
instead of one per element, thus saving (at most) 20 bytes per rendered location.

* On average, one @position change@ command "costs" 9 bytes ("\ESC[150;42H").
Sorting the "color group" by positions increases the likelyhood that
two consecutive elements are next to one another. In that case we can omit this
command, because after a 'putChar' the cursor location goes one step to the
right. (More to come on this subject, cf. the BACKLOG)

* And
<https://www.reddit.com/r/haskellquestions/comments/7i6hi5/optimizing_memory_usage_array_of_unboxed_values/
I found usefull answers on reddit> which helped in the process of optimizing
the memory layout. Contiguous memory blocks are mow used to store information
on rendered locations, and we use an Unboxed 'Word64', which is the most efficient Haskell type in terms of
"information qty / memory usage" ratio, to encode (from
higher bits to lower bits):

    * background color  (Word8)
    * foreground color  (Word8)
    * buffer position   (Word16)
    * unicode character (Word32)

Finally, if this module doesn't play well with your game graphics, let me know!
Maybe we'll need more optimizations options in the future, to adapt more closely
to the various kind of graphics that can be used in games.

= Usage

TODO stats on hamazed to see stdout buffer usage

-- TODO hide the render size API, replace it by an optional Initialize function
which will allocate the buffers

-- TODO provide a context which we can create using TerminalSize or CustomSize,
it contains the IORef, the buffers.

Setup the frame size. You can use
<https://hackage.haskell.org/package/terminal-size#readme terminal-size package>
to retrieve the current size of the terminal.

> setFrameDimensions 300 90

Draw

> setForegroundColor $ xterm256ColorToCode $ RGBColor $ RGB 0 2 3
> setDrawLocation 10 20
> drawStr "Hello world!"

Render to the console

> renderFrame True {-reset the back buffer to the initial colors-}

= Global states

The functions of this module are in the IO monad because a top level
IORef is used to store states : current colors, current draw position,
front and back buffers, and internal buffers.

TODO should this module handle cursor hiding?

-- TODO include in this module functions to create colors to be able to write

> rgb 3 4 5
-}
module Render.Backends.Internal.Delta
       (
       -- * Setup
         newContext
       , Context
       , setFrameDimensions
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

import           GHC.Show(showString)

import qualified Prelude ( putChar, putStr )

import           Data.Colour.SRGB( RGB(..) )
import           Data.IORef( IORef , newIORef , readIORef , writeIORef )
import           Data.Maybe( fromMaybe )
import           Data.String( String )
import           Data.Text( Text, unpack )
import           Data.Vector.Algorithms.Intro -- unstable sort
import           Data.Vector.Unboxed.Mutable( IOVector, replicate, read, write, unzip )
import           Data.Word( Word32, Word16 )

import           System.Console.ANSI( xterm256ColorToCode, Color8Code(..), Xterm256Color(..)
                                    , setCursorPosition, setSGRCode, SGR(..), ConsoleLayer(..) )
import           System.IO( stdout, hFlush )

import           Render.Backends.Internal.BufferCell
import           Render.Backends.Internal.Types
import qualified Render.Backends.Internal.UnboxedDynamic as Dyn
                                ( IOVector, accessUnderlying, length
                                , new, read, clear, pushBack )


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
setFrameDimensions :: Word16
                    -- ^ Width
                    -> Word16
                    -- ^ Height
                    -> Context
                    -> IO ()
setFrameDimensions width height (Context ref) = do
  (RenderState (Pencil idx colors) (Buffers _ _ _ prevWidth prevHeight _)) <- readIORef ref
  if prevWidth == width && prevHeight == height
    then
      return ()
    else do
      buffers@(Buffers _ _ size _ _ _) <- mkBuffers width height
      let newPencilIdx = idx `fastMod` size
      writeIORef ref $ RenderState (Pencil newPencilIdx colors) buffers


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

newtype Context = Context {
  _contextState :: IORef RenderState
} deriving(Eq)

instance Show Context where
  showsPrec _ _ = showString "IORef"

{-# NOINLINE newContext #-}
newContext :: IO Context
newContext =
  mkBuffers 300 80 -- TODO should we use terminal-size?
    >>= \buffers ->
      newIORef (RenderState mkInitialState buffers)
        >>= return . Context

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


setDrawColor :: ConsoleLayer
             -> Color8Code
             -> Context
             -> IO Colors
setDrawColor layer color (Context screenBuffer) = do
  (RenderState (Pencil idx prevColors@(Colors prevBg prevFg)) b ) <- readIORef screenBuffer
  let newColors = case layer of
        Foreground -> Colors prevBg color
        Background -> Colors color prevFg
  writeIORef screenBuffer $ RenderState (Pencil idx newColors) b
  return prevColors


setDrawColors :: Colors
              -> Context
              -> IO Colors
setDrawColors colors (Context screenBuffer) = do
  (RenderState (Pencil idx prevColors) b ) <- readIORef screenBuffer
  writeIORef screenBuffer $ RenderState (Pencil idx colors) b
  return prevColors


restoreDrawColors :: Colors
                  -> Context
                  -> IO ()
restoreDrawColors colors c = void $ setDrawColors colors c


setDrawLocation :: Int
                -> Int
                -> Context
                -> IO ()
setDrawLocation x y ctxt
  | x < 0 || y < 0 = error $ "cannot draw to negative location " ++ show (x,y)
  | otherwise      = setDrawLocation' (fromIntegral x) (fromIntegral y) ctxt


setDrawLocation' :: Word16
                 -> Word16
                 -> Context
                 -> IO ()
setDrawLocation' x y (Context screenBuffer) = do
  (RenderState (Pencil _ colors) b) <- readIORef screenBuffer
  let idx = indexFromXY b x y
  writeIORef screenBuffer $ RenderState (Pencil idx colors) b


-- | Draws a 'Char' at the current draw location, with the current draw colors.
drawChar :: Char
         -> Context
         -> IO ()
drawChar c (Context screenBuffer) = do
  (RenderState (Pencil idx colors) (Buffers back _ _ _ _ _)) <- readIORef screenBuffer
  putCharRawAt back idx colors c


putCharRawAt :: Buffer Back
             -> Word16
             -> Colors
             -> Char
             -> IO ()
putCharRawAt back idx colors c =
  writeToBack back idx (mkCell colors c)


-- | Draws multiple times the same 'Char' starting from the current draw location,
--   using current draw colors.
drawChars :: Int
          -- ^ Number of repetitions.
          -> Char
          -> Context
          -> IO ()
drawChars count c (Context screenBuffer) = do
  (RenderState (Pencil idx colors) (Buffers back _ size _ _ _)) <- readIORef screenBuffer
  let cell = mkCell colors c
  mapM_ (\i -> let pos = (idx + fromIntegral i) `fastMod` size
               in writeToBack back pos cell) [0..pred count]


-- | Draws a 'String' starting from the current draw location,
--   using current draw colors.
drawStr :: String
        -> Context
        -> IO ()
drawStr str (Context screenBuffer) = do
  (RenderState (Pencil idx colors) (Buffers back _ size _ _ _)) <- readIORef screenBuffer
  mapM_ (\(c, i) -> putCharRawAt back (idx+i `fastMod` size) colors c) $ zip str [0..]


-- | Draws a 'Text' starting from the current draw location,
--   using current draw colors.
drawTxt :: Text
        -> Context
        -> IO ()
drawTxt text = drawStr $ unpack text


{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Word16 -> Cell -> IO ()
writeToBack (Buffer b) pos = write b (fromIntegral pos)


-- | Fills the entire canvas with colors and a char.
fill :: Maybe Colors
     -- ^ When Nothing, the initial colors are used.
     -> Maybe Char
     -- ^ When Nothing, a space character is used.
     -> Context
     -> IO ()
fill mayColors mayChar ctxt@(Context screenBuffer) = do
  let colors = fromMaybe initialColors mayColors
      char = fromMaybe ' ' mayChar
  _ <- setDrawColors colors ctxt
  (RenderState _ b) <- readIORef screenBuffer
  fillBackBuffer b colors char


fillBackBuffer :: Buffers
               -> Colors
               -> Char
               -> IO ()
fillBackBuffer (Buffers (Buffer b) _ size _ _ _) colors char = do
  let cell = mkCell colors char
  mapM_ (\pos -> write b pos cell) [0..fromIntegral $ pred size]


{- | This function does the following:

* Store the difference between front and back buffer in an auxiliary buffer.
* Copy the back buffer to the front buffer
* Reset (clear) the content of the back buffer
* Sort the auxiliary buffer by color first, then by positions.
* Sends rendering commands to the stdout buffer, based on the drawing sequence
defined by the sorted auxiliary buffer.
* Flush stdout.
-}
renderFrame :: Bool
              -- ^ If True, after submission the drawing is reset to the initial color.
              -- This can also be achieved by calling "fill Nothing Nothing" after this call,
              -- at the cost of one more traversal of the back buffer.
              -> Context
              -> IO ()
renderFrame clearBack (Context screenBuffer) = do
  (RenderState _ buffers@(Buffers _ _ _ _ _ (Delta delta))) <- readIORef screenBuffer
  computeDelta buffers 0 clearBack
  szDelta <- Dyn.length delta
  underlying <- Dyn.accessUnderlying delta

  -- On average, foreground and background color change command is 20 bytes :
  --   "\ESC[48;5;167;38;5;255m"
  -- On average, position change command is 9 bytes :
  --   "\ESC[150;42H"
  -- So we want to minimize the number of color changes first, and then mimnimize
  -- the number of position changes.
  -- In 'Cell', color is encoded in higher bits than position, so this sort
  -- sorts by color first, then by position, which is what we want.
  sort underlying

  -- We ignore this color value. We could store it and use it to initiate the recursion
  -- at next render but if the client renders with another library in-betweeen, this value
  -- would be wrong, so we can ignore it here for more robustness.
  _ <- renderDelta szDelta 0 Nothing Nothing buffers
  Dyn.clear delta
  hFlush stdout -- TODO could that be async?

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


-- [before doing this micro optimization, verify that there is no side effect,
-- it could take longer to render]
-- TODO: optimize position changes which are 9 bytes on average :
-- if the distance between 2 consecutive differences (of the same row) is < 9,
-- and on the path there is no needed color change, we can avoid a position
-- change command and add all items of the path to the delta vector instead.
-- This requires that we mimic the logic of drawCell
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
      -- read from back buffer
      valueToDisplay <- read backBuf i
      -- clear back buffer
      when (clearBack && (initialCell /= valueToDisplay)) $ write backBuf i initialCell
      -- read from front buffer
      valueCurrentlyDisplayed <- read frontBuf i
      -- if differences are found, update front buffer and push the difference
      -- in delta vector
      when (valueToDisplay /= valueCurrentlyDisplayed) $ do
          write frontBuf i valueToDisplay
          Dyn.pushBack delta $ mkIndexedCell valueToDisplay idx
      -- recurse
      computeDelta b (succ idx) clearBack

-- | The command to set the cursor position to 123,45 is "\ESC[123;45H",
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
