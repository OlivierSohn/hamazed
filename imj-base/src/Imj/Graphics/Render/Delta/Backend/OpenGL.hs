{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.Render.Delta.Backend.OpenGL
    ( newOpenGLBackend
    , OpenGLBackend
    , PreferredScreenSize(..)
    , mkFixedScreenSize
    , windowCloseCallback -- for doc
    ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Concurrent.MVar.Strict(MVar, newMVar, swapMVar, readMVar)
import           Control.Concurrent.STM(TQueue, atomically, newTQueueIO, writeTQueue)
import           Control.DeepSeq(NFData)
import           Data.Char(isHexDigit, digitToInt)
import           Data.Text(pack)
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Foreign.Ptr(nullPtr)

import           Data.Vector.Unboxed.Mutable(read)
import qualified Imj.Data.Vector.Unboxed.Mutable.Dynamic as Dyn
                        (IOVector, accessUnderlying, length)

import           Imj.Geo.Continuous.Types
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Font
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Env
import           Imj.Graphics.Render.Delta.Internal.Types
import           Imj.Graphics.Render.Delta.Cell
import           Imj.Input.Types
import           Imj.Log

charCallback :: TQueue PlatformEvent -> GLFW.Window -> Char -> IO ()
charCallback q _ c = atomically $ writeTQueue q $ KeyPress $ AlphaNum c

keyCallback :: TQueue PlatformEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback q _ k _ GLFW.KeyState'Pressed _ =
  case glfwKeyToKey k of
    Unknown -> return ()
    key -> atomically $ writeTQueue q $ KeyPress key
keyCallback _ _ _ _ _ _ = return ()

windowCloseCallback :: TQueue PlatformEvent -> GLFW.Window -> IO ()
windowCloseCallback keyQueue _ =
  atomically $ writeTQueue keyQueue StopProgram

data OpenGLBackend = OpenGLBackend {
    _glfwWin :: {-# UNPACK #-} !GLFW.Window
  , _glfwKeyEvts :: {-# UNPACK #-} !(TQueue PlatformEvent)
  , _ppu :: {-# UNPACK #-} !PPU
  -- ^ Number of pixels per discrete unit length. Keep it even for best results.
  , _windowSize :: {-# UNPACK #-} !Size
  -- ^ In number of pixels.
  , _fonts :: {-# UNPACK #-} !(MVar RenderingOptions)
  -- ^ Mutable rendering options
}

data RenderingOptions = RenderingOptions {
    _cycleIndex :: {-# UNPACK #-} !Int
  , _style :: {-unpack sum-} !RenderingStyle
  , _getFonts :: !Fonts
} deriving(Generic, Show, NFData)
instance PrettyVal RenderingOptions where
  prettyVal opt = prettyVal ("RenderingOptions:" :: String
                            , show opt)

data RenderingStyle = AllFont
                    | HexDigitAsBits
                    deriving(Generic, NFData, Eq, Show, PrettyVal)

instance DeltaRenderBackend OpenGLBackend where
    render (OpenGLBackend win _ ppu size mFont) d w =
      liftIO $ readMVar mFont >>= \(RenderingOptions _ rs fonts) ->
        deltaRenderOpenGL win ppu size fonts rs d w

    cleanup (OpenGLBackend win _ _ _ _) = liftIO $ destroyWindow win

    cycleRenderingOption (OpenGLBackend _ _ ppu _ mRO) =
      liftIO $
        readMVar mRO >>= cycleRenderingOptions ppu >>= either
          (return . Left)
          (\v -> do
              void $ swapMVar mRO v
              return $ Right ())

    getDiscreteSize (OpenGLBackend _ _ (Size ppuH ppuW) (Size h w) _) =
      return $ Just $ Size
        (fromIntegral $ quotCeil (fromIntegral h) (fromIntegral ppuH :: Int))
        (fromIntegral $ quotCeil (fromIntegral w) (fromIntegral ppuW :: Int))
    {-# INLINABLE render #-}
    {-# INLINABLE cleanup #-}
    {-# INLINABLE getDiscreteSize #-}

{-# INLINABLE quotCeil #-}
quotCeil :: (Integral a) => a -> a -> a
quotCeil x y =
  let (q,r) = quotRem x y
  in if r == 0
       then q
       else q + 1

instance PlayerInput OpenGLBackend where
  programShouldEnd (OpenGLBackend win _ _ _ _) = liftIO $ GLFW.windowShouldClose win
  plaformQueue (OpenGLBackend _ q _ _ _) = q
  pollKeys _ = liftIO GLFW.pollEvents
  waitKeysTimeout _ = liftIO . GLFW.waitEventsTimeout . unsafeToSecs
  queueType _ = ManualFeed

  {-# INLINABLE programShouldEnd #-}
  {-# INLINABLE plaformQueue #-}
  {-# INLINABLE pollKeys #-}
  {-# INLINABLE waitKeysTimeout #-}
  {-# INLINABLE queueType #-}

glfwKeyToKey :: GLFW.Key -> Key
glfwKeyToKey GLFW.Key'Enter  = Enter
glfwKeyToKey GLFW.Key'Escape = Escape
glfwKeyToKey GLFW.Key'Tab    = Tab
glfwKeyToKey GLFW.Key'Delete = Delete
glfwKeyToKey GLFW.Key'Backspace = BackSpace
glfwKeyToKey GLFW.Key'Right = Arrow RIGHT
glfwKeyToKey GLFW.Key'Left  = Arrow LEFT
glfwKeyToKey GLFW.Key'Down  = Arrow Down
glfwKeyToKey GLFW.Key'Up    = Arrow Up
glfwKeyToKey _ = Unknown

-- there are 3 notions of window size here:
-- - the preferred size
-- - the preferred size rounded to a multiple of ppu
-- - the actual size of the window
newOpenGLBackend :: String -> PPU -> PreferredScreenSize -> IO (Either String OpenGLBackend)
newOpenGLBackend title ppu preferred = do
  q <- newTQueueIO
  let simpleErrorCallback e x =
        atomically $ writeTQueue q $ Message Warning $
          "Warning or error from glfw backend: " <> pack (show (e,x))
  GLFW.setErrorCallback $ Just simpleErrorCallback

  GLFW.init >>= \case
    False -> error "could not initialize GLFW"
    True -> return ()

  let maySize = case preferred of
        FixedScreenSize s -> Just $ floorToPPUMultiple s ppu
        FullScreen -> Nothing
  win <- createWindow title maySize
  GLFW.setKeyCallback win $ Just $ keyCallback q
  GLFW.setCharCallback win $ Just $ charCallback q
  GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback q
  GLFW.pollEvents -- this is necessary to show the window
  (w,h) <- GLFW.getWindowSize win
  let actualSize = Size (fromIntegral h) (fromIntegral w)
  createFonts 0 ppu >>= either
    (return . Left)
    (\fonts -> Right . OpenGLBackend win q ppu actualSize <$>
        newMVar (RenderingOptions 0 AllFont fonts))


cycleRenderingOptions :: PPU -> RenderingOptions -> IO (Either String RenderingOptions)
cycleRenderingOptions ppu (RenderingOptions idx rs fonts) = do
  let newRo = case rs of
        AllFont -> HexDigitAsBits
        HexDigitAsBits -> AllFont
      newIdx = succ idx
      (q,r) = quotRem newIdx 2
      nFonts = length fontFiles
  fmap (RenderingOptions newIdx newRo)
    <$> if 0 == r && nFonts > 1
          then
            createFonts (q `mod` nFonts) ppu
          else
            return $ Right fonts

createWindow :: String -> Maybe Size -> IO GLFW.Window
createWindow title s = do
  (mon, Size (Length height) (Length width)) <- maybe
    ( -- full screen mode
      GLFW.getPrimaryMonitor >>= maybe
        (return (Nothing, Size 600 1200))
        (\mon -> GLFW.getVideoMode mon >>= maybe
          (return (Nothing, Size 600 1200))
          (\mode -> do
              GLFW.windowHint $ GLFW.WindowHint'RedBits     $ GLFW.videoModeRedBits mode
              GLFW.windowHint $ GLFW.WindowHint'GreenBits   $ GLFW.videoModeGreenBits mode
              GLFW.windowHint $ GLFW.WindowHint'BlueBits    $ GLFW.videoModeBlueBits mode
              GLFW.windowHint $ GLFW.WindowHint'RefreshRate $ GLFW.videoModeRefreshRate mode
              return (Just mon
                    , Size (fromIntegral $ GLFW.videoModeHeight mode)
                           (fromIntegral $ GLFW.videoModeWidth mode))
              )))
    (\s' -> return (Nothing, s'))
    s
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  -- Single buffering goes well with delta-rendering, hence we use single buffering.
  GLFW.windowHint $ GLFW.WindowHint'DoubleBuffer False
  m <- GLFW.createWindow width height title mon Nothing
  let win = fromMaybe (error $ "could not create a GLFW window of size " ++ show s) m
  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
  GLFW.makeContextCurrent (Just win)

  GL.clearColor GL.$= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.finish
  --GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0 -- light position
  --GL.light    (GL.Light 0) GL.$= GL.Enabled          -- enable light
  --GL.lighting   GL.$= GL.Enabled
  --GL.cullFace   GL.$= Just GL.Back
  --GL.depthFunc  GL.$= Just GL.Less
  return win

destroyWindow :: GLFW.Window -> IO ()
destroyWindow win = do
  GLFW.destroyWindow win
  GLFW.terminate

deltaRenderOpenGL :: GLFW.Window
                  -> PPU
                  -> Size
                  -> Fonts
                  -> RenderingStyle
                  -> Delta
                  -> Dim Width
                  -> IO (Time Duration System, Time Duration System)
deltaRenderOpenGL _ ppu size fonts rs (Delta delta) w = do
  t1 <- getSystemTime
  renderDelta ppu size fonts rs delta w
  t2 <- getSystemTime
  -- To make sure all commands are visible on screen after this call, since we are
  -- in single-buffer mode, we use glFinish:
  GL.finish
  t3 <- getSystemTime
  return (t1...t2,t2...t3)

renderDelta :: PPU
            -> Size
            -> Fonts
            -> RenderingStyle
            -> Dyn.IOVector Cell
            -> Dim Width
            -> IO ()
renderDelta ppu size fonts rs delta' w = do
  sz <- Dyn.length delta'
  delta <- Dyn.accessUnderlying delta'
      -- We pass the underlying vector, and the size instead of the dynamicVector
  let renderDelta' :: Dim BufferIndex -> IO ()
      renderDelta' index
       | fromIntegral sz == index = return ()
       | otherwise = do
          c <- read delta $ fromIntegral index
          let (bg, fg, idx, glyph) = expandIndexed c
              (char,fontIndex) = decodeGlyph glyph
              font = lookupFont fontIndex fonts
              (x,y) = xyFromIndex w idx
          draw ppu size font rs x y char bg fg
          renderDelta' $ succ index
  void (renderDelta' 0)

draw :: PPU
     -> Size
     -> Font
     -> RenderingStyle
     -> Dim Col
     -> Dim Row
     -> Char
     -> Color8 Background -> Color8 Foreground -> IO ()
draw ppu size font rs col row char bg fg
  | rs == HexDigitAsBits && isHexDigit char = do
    let n = digitToInt char
        (d,d') = quotRem n 8
        (c,c') = quotRem d' 4
        (b,a) = quotRem c' 2
        colorOn  = GL.Color3 1 1 0       -- yellow
        colorOff = GL.Color3 0.4 0.4 0.4 -- grey
        bits = [a,b,c,d]
        locations = [(0,0),(1,0),(1,1),(0,1)]
    mapM_
      (\((dx, dy), bit) -> do
        let color
              | bit == 0 = colorOff
              | otherwise = colorOn
        drawSquare (half ppu) size (2 * fromIntegral col + dx)
                                   (2 * fromIntegral row + dy) color)
      $ zip locations bits
  | otherwise = do
      drawSquare ppu size (fromIntegral col) (fromIntegral row) $ GL.Color3 bR bG (bB :: GL.GLfloat)
      renderChar font ppu size col row char $ GL.Color3 fR fG (fB :: GL.GLfloat)
 where
   (bR,bG,bB) = color8ToUnitRGB bg
   (fR,fG,fB) = color8ToUnitRGB fg


renderChar :: Font -> PPU -> Size -> Dim Col -> Dim Row -> Char -> GL.Color3 GL.GLfloat -> IO ()
renderChar (Font font (Vec2 offsetCol offsetRow)) (Size ppuH ppuW) (Size winHeight winWidth) c r char color = do
  let unit x ppu = 2 * fromIntegral ppu / x
      totalH = fromIntegral $ quotCeil (fromIntegral winHeight) ppuH
      unitRow x = -1 + offsetRow * 2 / fromIntegral winHeight + unit (fromIntegral winHeight) ppuH * (totalH - x)
      unitCol x = -1 + offsetCol * 2 / fromIntegral winWidth  + unit (fromIntegral winWidth)  ppuW * x
  -- opengl coords interval [-1, 1] represent winLength pixels, hence:
  --   pixelLength = 2 / winlength
  --
  -- R = 'font offset' * pixelLength
  --
  -- with nUnits = quotCeil winLength ppu:
  --
  -- c,r are in [0..pred nUnits]
  -- 1+r is in [1..nUnits]
  --
  -- unitCol c     = -1 + R + 2 * (            c / nUnits) : is in [-1 + R, -1 + R + 2 * (1-1/nUnits)]
  --                                                             = [-1 + R,  1 + R - 2/nUnits        ]
  --
  -- unitRow (1+r) = -1 + R + 2 * (nUnits-(1+r)) / nUnits
  --               = -1 + R + 2 * (1 - (1+r)/nUnits)       : is in [-1 + R, -1 + R + 2 * (1 - 1/nUnits)]
  --                                                             = [-1 + R,  1 + R - 2/nUnits          ]
  --
  -- Hence, the raster position is put at unit rectangle corners, offset by a number of pixels
  -- both in horizontal and vertical directions.
  let x = unitCol (fromIntegral c :: Float) :: GL.GLfloat
      y = unitRow (fromIntegral (succ r) :: Float)
  -- From <https://www.khronos.org/opengl/wiki/Coloring_a_bitmap>:
  -- The present raster color is locked by the use glRasterPos*()
  --
  -- Hence, the color must be set /before/ calling 'rasterPos':
  GL.color color
  rasterPos x y

  FTGL.renderFont font [char] FTGL.Front --FTGL.Side

{-# INLINE rasterPos #-}
rasterPos :: GL.GLfloat -> GL.GLfloat -> IO ()
rasterPos !x' !y' = do
  GL.rasterPos $ GL.Vertex2 x y
  when (dx/=0 || dy/=0) $
    -- <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glBitmap.xml>
    -- To set a valid raster position outside the viewport,
    -- first set a valid raster position inside the viewport,
    -- then call glBitmap with NULL as the bitmap parameter and with
    -- xmove and ymove set to the offsets of the new raster position. This technique is useful when panning an image around the viewport.
    GL.bitmap (GL.Size 0 0) (GL.Vertex2 0 0) (GL.Vector2 dx dy) nullPtr
 where
  (dx,x) = split x'
  (dy,y) = split y'
  split a
    | a >  1    = (a-1, 1)
    | a < -1    = (a+1,-1)
    | otherwise = (  0, a)

drawSquare :: PPU -> Size -> Float -> Float -> GL.Color3 GL.GLfloat -> IO ()
drawSquare (Size ppuH ppuW) (Size winHeight winWidth) c r color = do
  let vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GL.GLfloat)
      unit x ppu = 2 * fromIntegral ppu / x
      totalH = fromIntegral $ quotCeil (fromIntegral winHeight) ppuH
      unitRow x = -1 + unit (fromIntegral winHeight) ppuH * (totalH - x)
      unitCol x = -1 + unit (fromIntegral winWidth)  ppuW * x

      -- with nUnits = quotCeil winLength ppu:
      --
      -- c,r are in [0..pred nUnits]
      -- 1+c,1+r are in [1..nUnits]
      --
      -- They /represent/ corners of unit rectangles paving the screen.
      --
      -- unitCol c = -1 + 2 * (           c / nUnits)    : is in [-1         , -1 + 2 * (1-1/nUnits)]
      --                                                       = [-1         , 1 - 2/nUnits         ]
      -- unitCol (1+c)                                   : is in [-1+2/nUnits, 1                    ]

      -- unitRow r = -1 + 2 * ((nUnits - r) / nUnits)    : is in [-1+2/nUnits, 1           ]
      -- unitRow (1+r)                                   : is in [-1         , 1 - 2/nUnits]
      --
      -- Hence, the screen is paved from -1-1 to 1 1 with rectangles, and coordinates
      -- passed to vertex3f are at pixel /corners/.
      --
      -- Note that when drawing a triangle, not all vertices have to be inside the viewport
      -- so unlike in 'renderChar', we don't need to compensate for coordinates outside [-1,1].
      (r1,r2) = (unitRow r, unitRow (1 + r))
      (c1,c2) = (unitCol c, unitCol (1 + c))
  GL.renderPrimitive GL.TriangleFan $ do
    GL.color color
    vertex3f c2 r1 0
    vertex3f c2 r2 0
    vertex3f c1 r2 0
    vertex3f c1 r1 0


data PreferredScreenSize =
    FixedScreenSize !Size
  | FullScreen
  deriving(Eq, Show)

mkFixedScreenSize :: Int -> Int -> Either String PreferredScreenSize
mkFixedScreenSize w h =
  Right $ FixedScreenSize $ Size (fromIntegral h) (fromIntegral w)
