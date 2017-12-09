{-# LANGUAGE NoImplicitPrelude #-}

-- | This module can be used by games that need to render in the console
-- without tearing or flickering effects.
--
-- Using a double-buffering technique, at each frame we compute the delta
-- between the previous and the current frame. We just render the delta, ie
-- the parts of the screen that changed.
--
-- This technique allows to have simple rendering code in the game (the game
-- developper will not have to worry about drawing something or not, s/he can
-- draw always everything at each frame), and results in flickering/tearing-free
-- renderihg, because since we send very few rendering instructions to the console,
-- we control precisely when the stdout flush occurs (else, with naive rendering,
-- intermediate flushes will occur when the buffer limits are reached, resulting
-- in tearing effect).
--
-- This way, we always see one full frame rendered in the console.
--
-- The functions of this module are mainly in the IO monad because a top level
-- IORef is used to store states : current colors, current draw position and
-- internal front and back buffers.

module Render.Backends.Internal.Delta
       (
       -- * Setup
         setCanvasDimensions
       -- * Draw globally
       , fill
       -- * Draw locally
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
       , submitDrawing
       -- * Reexports
       , module Render.Backends.Internal.Types
       ) where


import           Imajuscule.Prelude hiding (replicate)

import qualified Prelude ( putChar, putStr )

import           Data.Vector.Unboxed.Mutable( IOVector, replicate, read, write, unzip )

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

-- | Buffer types
data Back
data Front

-- that could fit in a Word64 (Color8Code = 2 Word8, index = Word32)
-- with 16 bits extra room
data Pencil = Pencil {
    _pencilBufferIndex :: {-# UNPACK #-} !Word32
  , _pencilColors :: {-# UNPACK #-} !Colors
}

-- that could fit exactly in a Word64 (width and height = 2 Word16, size = Word32)
data Buffers = Buffers {
    _renderStateBackBuffer :: !(Buffer Back)
  , _renderStateFrontBuffer :: !(Buffer Front)
  , _buffersSize :: !Word32
  , _buffersWidth :: !Word16
  , _buffersHeight :: !Word16
}


-- | Resizes the canvas, if needed
--  (this function does nothing if it was previously called with the same arguments).
setCanvasDimensions :: Word16
                    -- ^ Width
                    -> Word16
                    -- ^ Height
                    -> IO ()
setCanvasDimensions width height = do
  (RenderState (Pencil idx colors) (Buffers _ _ _ prevWidth prevHeight)) <- readIORef screenBuffer
  if prevWidth == width && prevHeight == height
    then
      return ()
    else do
      buffers@(Buffers _ _ size _ _) <- mkBuffers width height
      let newPencilIdx = idx `fastMod` size
      writeIORef screenBuffer $ RenderState (Pencil newPencilIdx colors) buffers


-- | Creates buffers for given width and height, replaces 0 width or height by 1.
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
  in assert (mkCell initialColors ' ' == res) res


{-# INLINE initialColors #-}
initialColors :: Colors
initialColors = Colors initialForeground initialBackground


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


setDrawColor :: ConsoleLayer -> Color8Code -> IO Colors
setDrawColor layer color = do
  (RenderState (Pencil idx prevColors@(Colors prevFg prevBg)) b ) <- readIORef screenBuffer
  let newColors = case layer of
        Foreground -> Colors color prevBg
        Background -> Colors prevFg color
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
  let idx = positionFromXY b x y
  writeIORef screenBuffer $ RenderState (Pencil idx colors) b


-- | Draws a 'Char' at the current draw location, with the current draw colors.
drawChar :: Char
         -> IO ()
drawChar c = do
  (RenderState (Pencil idx colors) (Buffers back _ _ _ _)) <- readIORef screenBuffer
  putCharRawAt back idx colors c


putCharRawAt :: Buffer Back -> Word32 -> Colors -> Char -> IO ()
putCharRawAt back idx colors c =
  writeToBack back idx $ mkCell colors c


-- | Draws multiple times the same 'Char' starting from the current draw location,
--   using current draw colors.
drawChars :: Int
          -- ^ Number of repetitions.
          -> Char
          -> IO ()
drawChars count c = do
  (RenderState (Pencil idx colors) (Buffers back _ size _ _)) <- readIORef screenBuffer
  let cell = mkCell colors c
  mapM_ (\i -> let pos = (idx + fromIntegral i) `fastMod` size
               in writeToBack back pos cell) [0..pred count]


-- | Draws a 'String' starting from the current draw location,
--   using current draw colors.
drawStr :: String -> IO ()
drawStr str = do
  (RenderState (Pencil idx colors) (Buffers back _ size _ _)) <- readIORef screenBuffer
  mapM_ (\(c, i) -> putCharRawAt back (idx+i `fastMod` size) colors c) $ zip str [0..]


-- | Draws a 'Text' starting from the current draw location,
--   using current draw colors.
drawTxt :: Text -> IO ()
drawTxt text = drawStr $ unpack text


{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Word32 -> Cell -> IO ()
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
fillBackBuffer (Buffers (Buffer b) _ size _ _) colors char = do
  let cell = mkCell colors char
  mapM_ (\pos -> write b pos cell) [0..fromIntegral $ pred size]


-- | Renders to the console the difference between the current drawing
--  and the last submitted drawing.
submitDrawing :: Bool
              -- ^ If True, after submission the drawing is reset to the initial color.
              -- This can also be achieved by calling "fill Nothing Nothing" after this call,
              -- at the cost of one more traversal of the back buffer.
              -> IO ()
submitDrawing clearBack = do
  (RenderState _ buffers) <- readIORef screenBuffer
  renderDelta buffers 0 clearBack Nothing
  hFlush stdout


renderDelta :: Buffers
            -> Word32
            -- ^ the position in the buffer
            -> Bool
            -- ^ should we clear the buffer containing the new values?
            -> Maybe Colors
            -- ^ The current console color, if it is known
            -> IO ()
renderDelta
 b@(Buffers (Buffer backBuf) (Buffer frontBuf) size _ _)
 idx
 clearBack
 mayCurrentConsoleColor
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
      renderDelta b (succ idx) clearBack (mayNewConsoleColor <|> mayCurrentConsoleColor)


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
