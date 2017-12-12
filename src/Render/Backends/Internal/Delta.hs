{-# LANGUAGE NoImplicitPrelude #-}

{-|
= Purpose

Render games or animations in the console, with a strong focus on avoiding
<https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.

= Design goals

* Minimize the amount of data sent to stdout buffer to render a frame.
* Design rendering algorithms that work well for all kinds of game graphics.
* Minimize memory footprint and run-time overhead.

= Features

* <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit colors> support
* <http://www.unicode.org/ Unicode> support

= Games using it

* <https://github.com/OlivierSohn/hamazed Hamazed>

= A simple example

>
> -- initialization
> ctxt <- createDefaultContext
>
> -- first frame
>
> let black = rgb 0 0 0
>     white = rgb 5 5 5
>     red   = rgb 5 0 0
>
> drawStr "Hello world!" (Coords (Row 10) (Col 20)) (LayeredColor black white) ctxt
> drawStr "How are you?" (Coords (Row 11) (Col 20)) (LayeredColor black white) ctxt
>
> flush ctxt
>
> -- second frame
>
> drawChars '_' 10 (Coords (Row 11) (Col 20)) (LayeredColor black red) ctxt
>
> flush ctxt
>

= History

I'll explain a bit of the context in which I developped this package.

In the beginnings of
<https://github.com/OlivierSohn/hamazed my first ascii based game>,
I implemented a naïve render loop which was:

* At the beginning of the frame, clear the console.
* Render the game elements to stdout.
* At the end of the frame, flush stdout.

The resulting graphics were fluid whilst a frame was ~100 different locations
in a single color. Yet, as color animations were introduced, flickering / tearing
effects started to occur on frames with more rendering locations.

This happened because stdout was saturated by rendering commands and would flush
before all commands corresponding to this frame were issued.

My first move to fix this was to maximize the size of stdout buffer with
<https://hackage.haskell.org/package/base-4.10.1.0/docs/System-IO.html#v:hSetBuffering hSetBuffering>
and
<https://hackage.haskell.org/package/base-4.10.1.0/docs/System-IO.html#v:BlockBuffering BlockBuffering>
parameter:

> hSetBuffering stdout $ BlockBuffering $ Just (maxBound :: Int)

Using 'testStdoutSizes' I measured that my stdout buffer was now 8096 bytes long,
instead of the default 2048 bytes.

This solved the problem, but only until I introduced more animations and more color changes
in the game.

I started thinking that I needed to keep the previous rendered frame and just draw
the difference between the previous and the current frame.

This is exactly what
<https://github.com/ibraimgm Rafael Ibraim> did to solve
<https://github.com/feuerbach/ansi-terminal/issues/5 this issue>.

With <https://gist.github.com/ibraimgm/40e307d70feeb4f117cd his code>,
the render loop becomes:

* Clear the /back/ /buffer/, but not the console!.
* Draw the game elements to the back buffer.
* Render the difference between the front and the back buffer to the console.
* Flush stdout.
* Copy the back buffer to the front buffer.

This approach fixed the rendering of my game, still, I wanted to continue
improving it to cover more use-cases:

* The most obvious optimization I could do was to group the rendered locations
by color before rendering them. On average, one @color change@ command "costs"
20 bytes ("\ESC[48;5;167;38;5;255m"), so grouping them saves a lot of bytes.

* Then, since one @position change@ command "costs" 9 bytes ("\ESC[150;42H") it's
also interesting to optimize for them.
Sorting the "color group" by positions increases the likelyhood that
two consecutive elements are next to one another, and in that case we can omit this
command entirely, because after a 'putChar' the cursor location goes one step to the
right. This also saved some bytes.

* Also
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

There will be more optimizations, and I'll try to keep them general enough so
that they benefit every use case.
-}
module Render.Backends.Internal.Delta
       (
       -- * Setup
         newContext
       , newDefaultContext
       , Buffers(..)
       , setResizePolicy
       , setClearPolicy
       -- * Fill the back buffer
       , fill
       -- * Draw to the back buffer
       , drawChar
       , drawChars
       , drawStr
       , drawTxt
       -- * Render to the console
       , flush
       -- * Reexports
       , module Color.Types
       , Coords(..)
       , IORef
       ) where


import           Imajuscule.Prelude hiding (replicate)

import qualified Prelude ( putChar, putStr )

import           Data.IORef( IORef , newIORef , readIORef , writeIORef )
import           Data.Maybe( fromMaybe )
import           Data.String( String )
import           Data.Text( Text, unpack )
import           Data.Vector.Algorithms.Intro -- unstable sort
import           Data.Vector.Unboxed.Mutable( replicate, read, write, unzip )

import           System.Console.ANSI( Color8Code(..)
                                    , setCursorPosition, setSGRCode, SGR(..), ConsoleLayer(..) )
import           System.IO( stdout, hFlush )

import           Color
import           Color.Types

import           Geo.Discrete.Types(Coords(..))

import           Render.Backends.Internal.BufferCell
import qualified Render.Backends.Internal.UnboxedDynamic as Dyn
                                ( accessUnderlying, length
                                , new, read, clear, pushBack )
import           Render.Types
import           Render.Backends.Internal.Types
import           Render.Backends.Internal.Dimensions


-- | Creates a context using default policies.
newDefaultContext :: IO (IORef Buffers)
newDefaultContext = newContext Nothing Nothing Nothing

-- | Creates a context using optional policies.
newContext :: Maybe ResizePolicy -> Maybe ClearPolicy -> Maybe ClearColor -> IO (IORef Buffers)
newContext mayResizePolicy mayClearPolicy mayClearColor = do
  let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
      clearColor = fromMaybe defaultClearColor mayClearColor
  newContext' $ Policies resizePolicy clearPolicy clearColor

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
          -> IO (Buffer Back, Buffer Front, Delta, Dim Size, Dim Width)
mkBuffers width' height' backBufferCell = do
  let (sz, width) = bufferSizeFromWH width' height'
      (Color8Code bg, Color8Code fg, char) = expand backBufferCell
      -- We initialize to different colors to force a first render to the whole console.
      frontBufferCell = mkCell (LayeredColor (Color8Code (succ bg)) (Color8Code (succ fg))) (succ char)
  buf <- newBufferArray sz (backBufferCell, frontBufferCell)
  delta <- Dyn.new $ fromIntegral sz -- reserve the maximum possible size
  let (back, front) = unzip buf
  return (Buffer back, Buffer front, Delta delta, sz, width)

defaultResizePolicy :: ResizePolicy
defaultResizePolicy = MatchTerminalSize

defaultClearColor :: Color8Code
defaultClearColor = black

defaultClearPolicy :: ClearPolicy
defaultClearPolicy = ClearAtEveryFrame

setResizePolicy :: IORef Buffers -> Maybe ResizePolicy -> IO ()
setResizePolicy ref mayResizePolicy =
  readIORef ref
    >>= \(Buffers a b c d e (Policies _ f g)) -> do
      let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
          buf = Buffers a b c d e (Policies resizePolicy f g)
      adjustSizeIfNeeded buf
        >>= writeIORef ref

adjustSizeIfNeeded :: Buffers -> IO Buffers
adjustSizeIfNeeded buffers@(Buffers _ _ prevSize prevWidth _ policies@(Policies resizePolicy _ _)) = do
  (width, height) <- getDimensions resizePolicy
  let prevHeight = getHeight prevWidth prevSize
  if prevWidth /= width || prevHeight /= height
    then
      createBuffers policies width height
    else
      return buffers

createBuffers :: Policies -> Dim Width -> Dim Height -> IO Buffers
createBuffers pol@(Policies _ _ clearColor) w h = do
  (newBack, newFront, newDelta, newSize, newWidth) <- mkBuffers w h (clearCell clearColor)
  -- no need to clear : we initialized with the right value
  return $ Buffers newBack newFront newSize newWidth newDelta pol

updateSize :: IORef Buffers -> IO ()
updateSize ref =
  readIORef ref >>= adjustSizeIfNeeded >>= writeIORef ref

setClearPolicy :: IORef Buffers -> Maybe ClearPolicy -> Maybe ClearColor -> IO ()
setClearPolicy ref mayClearPolicy mayClearColor =
  readIORef ref
    >>= \(Buffers a b c d e (Policies f _ _)) -> do
      let clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
          clearColor = fromMaybe defaultClearColor mayClearColor
      return (Buffers a b c d e (Policies f clearPolicy clearColor))
        >>= writeIORef ref

clearIfNeeded :: ClearContext -> Buffers -> IO ()
clearIfNeeded ctxt b@(Buffers _ _ _ _ _ (Policies _ clearPolicy clearColor)) = do
  let clear = fillBackBuffer b (clearCell clearColor)
  case clearPolicy of
    ClearAtEveryFrame -> clear
    ClearOnAllocationOnly ->
      case ctxt of
        OnAllocation -> clear
        OnFrame -> return ()

clearCell :: ClearColor -> Cell
clearCell clearColor =
  -- Any foreground color would be ok
  mkCell (LayeredColor clearColor white) ' '

-- TODO use phantom types for Cell (requires Data.Vector.Unboxed.Deriving to use newtype in vector)
newBufferArray :: Dim Size -> (Cell, Cell) -> IO BackFrontBuffer
newBufferArray size = replicate (fromIntegral size)

-- | Modulo optimized for cases where most of the time,
--    a < b (for a mod b)
{-# INLINE fastMod #-}
fastMod :: Int -> Dim Size -> Dim Index
fastMod a b'
  | 0 <= a && a < b = fromIntegral a          -- fast path
  | otherwise       = fromIntegral $ a `mod` b  -- slow path
  where b = fromIntegral b'


{-# INLINE indexFromPos #-}
indexFromPos :: Buffers -> Coords -> Dim Index
indexFromPos (Buffers _ _ size width _ _) (Coords y x) =
  (fromIntegral y * fromIntegral width + fromIntegral x) `fastMod` size


{-# INLINE xyFromIndex #-}
xyFromIndex :: Buffers -> Dim Index -> (Dim ColIndex, Dim RowIndex)
xyFromIndex (Buffers _ _ _ width _ _) idx =
  getRowCol idx width


drawChar :: Char
         -> Coords
         -> LayeredColor
         -> IORef Buffers
         -> IO (IORef Buffers)
drawChar c pos colors ioRefBuffers =
  readIORef ioRefBuffers
    >>= \b@(Buffers back _ _ _ _ _) -> do
      let idx = indexFromPos b pos
      writeToBack back idx (mkCell colors c)
      return ioRefBuffers


-- | Draws multiple times the same 'Char'
drawChars :: Int
          -- ^ Number of repetitions.
          -> Char
          -> Coords
          -> LayeredColor
          -> IORef Buffers
          -> IO (IORef Buffers)
drawChars count c pos colors ioRefBuffers =
  readIORef ioRefBuffers
    >>= \b@(Buffers back _ size _ _ _) -> do
      let cell = mkCell colors c
          idx = indexFromPos b pos
      mapM_
        (\i -> let idx' = (fromIntegral idx + i) `fastMod` size
               in writeToBack back idx' cell)
        [0..pred count]
      return ioRefBuffers


drawStr :: String
        -> Coords
        -> LayeredColor
        -> IORef Buffers
        -> IO (IORef Buffers)
drawStr str pos colors ioRefBuffers =
  readIORef ioRefBuffers
    >>= \b@(Buffers back _ size _ _ _) -> do
      let idx = indexFromPos b pos
      mapM_
        (\(c, i) ->
            writeToBack back (idx+i `fastMod` size) (mkCell colors c))
        $ zip str [0..]
      return ioRefBuffers


drawTxt :: Text
        -> Coords
        -> LayeredColor
        -> IORef Buffers
        -> IO (IORef Buffers)
drawTxt text = drawStr $ unpack text


{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Dim Index -> Cell -> IO ()
writeToBack (Buffer b) pos = write b (fromIntegral pos)


-- | Fills the entire canvas with a char.
fill :: Char
     -> LayeredColor
     -> IORef Buffers
     -> IO ()
fill char colors ioRefBuffers =
  readIORef ioRefBuffers
    >>= flip fillBackBuffer (mkCell colors char)


fillBackBuffer :: Buffers
               -> Cell
               -> IO ()
fillBackBuffer (Buffers (Buffer b) _ size _ _ _) cell =
  mapM_ (\pos -> write b pos cell) [0..fromIntegral $ pred size]


flush :: IORef Buffers -> IO ()
flush ioRefBuffers =
  readIORef ioRefBuffers
    >>=
      render
        >> do
          updateSize ioRefBuffers
          -- TODO if buffers resized because the terminal resized, send a clearScreen command or re-render with new size
          hFlush stdout -- TODO is flush blocking? slow? could it be async?



render :: Buffers -> IO ()
render buffers@(Buffers _ _ _ _ (Delta delta) _) = do
  computeDelta buffers 0

  clearIfNeeded OnFrame buffers

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
  _ <- renderDelta (fromIntegral szDelta) 0 Nothing Nothing buffers
  Dyn.clear delta

renderDelta :: Dim Size
            -> Dim Index
            -> Maybe LayeredColor
            -> Maybe (Dim Index)
            -> Buffers
            -> IO LayeredColor
renderDelta size index prevColors prevIndex
  b@(Buffers _ _ _ _ (Delta delta) _)
 | fromIntegral size == index = return $ LayeredColor (Color8Code 0) (Color8Code 0)
 | otherwise = do
    c <- Dyn.read delta $ fromIntegral index
    let (bg, fg, idx, char) = expandIndexed c
        prevRendered = maybe False (== pred idx) prevIndex
    setCursorPositionIfNeeded b idx prevRendered
    usedColor <- drawCell bg fg char prevColors
    renderDelta size (succ index) (Just usedColor) (Just idx) b


computeDelta :: Buffers
             -> Dim Index
             -- ^ the buffer index
             -> IO ()
computeDelta
 b@(Buffers (Buffer backBuf) (Buffer frontBuf) size _ (Delta delta) _)
 idx
  | fromIntegral idx == size = return ()
  | otherwise = do
      let i = fromIntegral idx
      -- read from back buffer
      valueToDisplay <- read backBuf i
      -- read from front buffer
      valueCurrentlyDisplayed <- read frontBuf i
      -- if differences are found, update front buffer and push the difference
      -- in delta vector
      when (valueToDisplay /= valueCurrentlyDisplayed) $ do
          write frontBuf i valueToDisplay
          Dyn.pushBack delta $ mkIndexedCell valueToDisplay idx
      -- recurse
      computeDelta b (succ idx)

-- | The command to set the cursor position to 123,45 is "\ESC[123;45H",
-- its size is 9 bytes : one order of magnitude more than the size
-- of a char, so we avoid sending this command when not strictly needed.
{-# INLINE setCursorPositionIfNeeded #-}
setCursorPositionIfNeeded :: Buffers
                          -> Dim Index
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
drawCell :: Color8Code -> Color8Code -> Char -> Maybe LayeredColor -> IO LayeredColor
drawCell bg fg char maybeCurrentConsoleColor = do
  let (bgChange, fgChange, usedFg) =
        maybe
          (True, True, fg)
          (\(LayeredColor bg' fg') ->
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
  return $ LayeredColor bg usedFg
