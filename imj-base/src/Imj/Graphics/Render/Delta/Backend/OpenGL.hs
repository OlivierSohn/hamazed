{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Graphics.Render.Delta.Backend.OpenGL
    ( newOpenGLBackend
    , OpenGLBackend
    , PreferredScreenSize(..)
    , mkScreenSize
    , windowCloseCallback -- for doc
    ) where

import           Imj.Prelude
import           Prelude(putStrLn, length)

import           Control.Concurrent.MVar.Strict(MVar, newMVar, swapMVar, readMVar)
import           Control.Concurrent.STM(TQueue, atomically, newTQueueIO, writeTQueue)
import           Control.DeepSeq(NFData)
import           Data.Char(isHexDigit, digitToInt)
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

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

charCallback :: TQueue Key -> GLFW.Window -> Char -> IO ()
charCallback q _ c = atomically $ writeTQueue q $ AlphaNum c

keyCallback :: TQueue Key -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback q _ k _ GLFW.KeyState'Pressed _ = case glfwKeyToKey k of
  Unknown -> return ()
  key -> atomically $ writeTQueue q key
keyCallback _ _ _ _ _ _ = return ()

windowCloseCallback :: TQueue Key -> GLFW.Window -> IO ()
windowCloseCallback keyQueue _ =
  atomically $ writeTQueue keyQueue StopProgram

data OpenGLBackend = OpenGLBackend {
    _glfwWin :: {-# UNPACK #-} !GLFW.Window
  , _glfwKeyEvts :: {-# UNPACK #-} !(TQueue Key)
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
  prettyVal opt = prettyVal ("RenderingOptions:", show opt)

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
      return $ Just $ Size (fromIntegral $ quot (fromIntegral h) ppuH)
                           (fromIntegral $ quot (fromIntegral w) ppuW)
    {-# INLINABLE render #-}
    {-# INLINABLE cleanup #-}
    {-# INLINABLE getDiscreteSize #-}

instance PlayerInput OpenGLBackend where
  programShouldEnd (OpenGLBackend win _ _ _ _) = liftIO $ GLFW.windowShouldClose win
  keysQueue (OpenGLBackend _ q _ _ _) = q
  pollKeys _ = liftIO GLFW.pollEvents
  waitKeysTimeout _ = liftIO . GLFW.waitEventsTimeout . unsafeToSecs
  queueType _ = ManualFeed

  {-# INLINABLE programShouldEnd #-}
  {-# INLINABLE keysQueue #-}
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
newOpenGLBackend title ppu (FixedScreenSize s) = do
  let simpleErrorCallback e x =
        putStrLn $ "Warning or error from glfw backend: " ++ unwords [show e, show x]
  GLFW.setErrorCallback $ Just simpleErrorCallback

  GLFW.init >>= \case
    False -> error "could not initialize GLFW"
    True -> return ()

  keyEventsChan <- newTQueueIO :: IO (TQueue Key)
  let roundedSize = floorToPPUMultiple s ppu
  win <- createWindow roundedSize title
  GLFW.setKeyCallback win $ Just $ keyCallback keyEventsChan
  GLFW.setCharCallback win $ Just $ charCallback keyEventsChan
  GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback keyEventsChan
  GLFW.pollEvents -- this is necessary to show the window
  (w,h) <- GLFW.getWindowSize win
  let actualSize = Size (fromIntegral h) (fromIntegral w)
  when (actualSize /= roundedSize) $ error $ "actual size is different from rounded size:" ++ show (actualSize, roundedSize)
  createFonts 0 ppu >>= either
    (return . Left)
    (\fonts -> Right . OpenGLBackend win keyEventsChan ppu actualSize <$>
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

createWindow :: Size -> String -> IO GLFW.Window
createWindow s@(Size (Length height) (Length width)) title = do
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  -- Single buffering goes well with delta-rendering, hence we use single buffering.
  GLFW.windowHint $ GLFW.WindowHint'DoubleBuffer False
  m <- GLFW.createWindow width height title Nothing Nothing
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
  let unit x ppu = 2 * recip (fromIntegral $ quot x ppu)
      totalH = fromIntegral $ quot (fromIntegral winHeight) ppuH
      -- with 6,5 there are leftovers on top of pipe, with 4 it's below.
      unitRow x = -1 + offsetRow * 2 / fromIntegral winHeight + unit (fromIntegral winHeight) ppuH * (totalH - x)
      unitCol x = -1 + offsetCol * 2 / fromIntegral winWidth  + unit (fromIntegral winWidth)  ppuW * x
  -- The present raster color is locked by the use glRasterPos*()
  -- <https://www.khronos.org/opengl/wiki/Coloring_a_bitmap>
  --
  -- Hence, we set the color /before/ raster pos:
  GL.color color

  -- opengl coords interval [-1, 1] represent winLength pixels, hence:
  --   pixelLength = 2 / winlength
  --
  -- R = 'font offset' * pixelLength
  --
  -- with nUnits = quot winLength ppu:
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

  GL.rasterPos $ GL.Vertex2 (unitCol (fromIntegral c :: Float) :: GL.GLfloat)
                            (unitRow (fromIntegral (succ r) :: Float))
  FTGL.renderFont font [char] FTGL.Front --FTGL.Side

drawSquare :: PPU -> Size -> Float -> Float -> GL.Color3 GL.GLfloat -> IO ()
drawSquare (Size ppuH ppuW) (Size winHeight winWidth) c r color = do
  let vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GL.GLfloat)
      unit x ppu = 2 * recip (fromIntegral $ quot x ppu)
      totalH = fromIntegral $ quot (fromIntegral winHeight) ppuH
      unitRow x = -1 + unit (fromIntegral winHeight) ppuH * (totalH - x)
      unitCol x = -1 + unit (fromIntegral winWidth)  ppuW * x

      -- with nUnits = quot winLength ppu:
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
      -- passed to vertex3f are at pixel /corners/, which is what we want.
      (r1,r2) = (unitRow r, unitRow (1 + r))
      (c1,c2) = (unitCol c, unitCol (1 + c))
  GL.renderPrimitive GL.TriangleFan $ do
    GL.color color
    vertex3f c2 r1 0
    vertex3f c2 r2 0
    vertex3f c1 r2 0
    vertex3f c1 r1 0


newtype PreferredScreenSize = FixedScreenSize Size

mkScreenSize :: Int -> Int -> Either String PreferredScreenSize
mkScreenSize w h =
  Right $ FixedScreenSize $ Size (fromIntegral h) (fromIntegral w)
