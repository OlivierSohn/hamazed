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
    , withFont
    , windowCloseCallback -- for doc
    ) where

import           Imj.Prelude

import           Control.Concurrent.MVar.Strict(MVar, newMVar, swapMVar, readMVar)
import           Control.Concurrent.STM(atomically, newTQueueIO, writeTQueue, tryPeekTQueue)
import           Data.ByteString(ByteString)
import           Data.Char(isHexDigit, digitToInt)
import           Data.Text(pack)
import           Data.IORef(newIORef, readIORef, writeIORef)
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Foreign.Ptr(nullPtr)
import           System.IO(print, putStrLn)
import           System.IO.Unsafe (unsafePerformIO)

import           Data.Vector.Unboxed.Mutable(unsafeRead)
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
charCallback q _ c = atomically $ writeTQueue q $ InterpretedKey $ AlphaNum c

keyCallback :: TQueue PlatformEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback q _ k _ s m = do
  atomically $ writeTQueue q $ StatefullKey k s m
  case s of
    GLFW.KeyState'Pressed ->
      maybe
        (return ())
        (atomically . writeTQueue q . InterpretedKey)
        $ glfwKeyToKey k
    _ -> return ()

glfwKeyToKey :: GLFW.Key -> Maybe Key
glfwKeyToKey GLFW.Key'Enter  = Just Enter
glfwKeyToKey GLFW.Key'Escape = Just Escape
glfwKeyToKey GLFW.Key'Tab    = Just Tab
glfwKeyToKey GLFW.Key'Delete = Just Delete
glfwKeyToKey GLFW.Key'Backspace = Just BackSpace
glfwKeyToKey GLFW.Key'Right = Just $ Arrow RIGHT
glfwKeyToKey GLFW.Key'Left  = Just $ Arrow LEFT
glfwKeyToKey GLFW.Key'Down  = Just $ Arrow Down
glfwKeyToKey GLFW.Key'Up    = Just $ Arrow Up
glfwKeyToKey _ = Nothing

windowCloseCallback :: TQueue PlatformEvent -> GLFW.Window -> IO ()
windowCloseCallback q _ =
  atomically $ writeTQueue q StopProgram

-- When resizing a window using a mouse drag of the borders, plenty of FramebufferSizeChanges
-- events are polled at once, at the end of the motion. Hence, to avoid slowing the app down
-- at that moment, we dedup.
framebufferSizeCallback :: TQueue PlatformEvent -> GLFW.Window -> Int -> Int -> IO ()
framebufferSizeCallback q _ _ _ = atomically $ tryPeekTQueue q >>= maybe
  write
  (\case
    FramebufferSizeChanges -> return ()
    _ -> write)
 where
  write = writeTQueue q FramebufferSizeChanges

data OpenGLBackend = OpenGLBackend {
    _glfwWin :: {-# UNPACK #-} !GLFW.Window
  , _glfwKeyEvts :: {-# UNPACK #-} !(TQueue PlatformEvent)
  , _fonts :: {-# UNPACK #-} !(MVar RenderingOptions)
  -- ^ Mutable rendering options
}

data RenderingOptions = RenderingOptions {
    _fontsVariations :: {-# UNPACK #-} !FontsVariations
  , _style :: {-unpack sum-} !RenderingStyle
  , _ppu :: {-# UNPACK #-} !PPU
  -- ^ Number of pixels per discrete unit length. Keep it even for best results when rendering numbers as squares.
  , _margin :: {-# UNPACK #-} !FontMargin
  , _getFonts :: !Fonts
} deriving(Generic, Show, NFData)
instance PrettyVal RenderingOptions where
  prettyVal opt = prettyVal ("RenderingOptions:" :: String
                            , show opt)

data RenderingStyle = AllFont
                    | HexDigitAsBits
                    deriving(Generic, NFData, Eq, Show, PrettyVal)


-- needed to workaround https://github.com/glfw/glfw/pull/1346
data HackState =
    NeedToMoveWindow
    -- ^ this is the first time the window was drawn:
    -- we need to move the window to force a window update by GLFW.
  | NeedToRestoreInitialPosition
    -- ^ restore the initial position, after the window has been moved.

hackState :: IORef (Maybe HackState)
{-# NOINLINE hackState #-}
hackState = unsafePerformIO (newIORef $ Just NeedToMoveWindow)

mojaveHack :: GLFW.Window -> IO ()
mojaveHack win =
  -- hack for OSX mojave to work around a GLFW limitation:
  -- see https://github.com/glfw/glfw/pull/1346
  readIORef hackState >>= maybe (return ()) (\case
    NeedToMoveWindow -> do
      move 1
      writeIORef hackState $ Just NeedToRestoreInitialPosition
    NeedToRestoreInitialPosition -> do
      move $ -1
      writeIORef hackState Nothing)
 where
  move n = do
    (x,y) <- GLFW.getWindowPos win
    GLFW.setWindowPos win (x + n) y

instance DeltaRenderBackend OpenGLBackend where
  render (OpenGLBackend win _ mFont) d w =
    liftIO $ readMVar mFont >>= \(RenderingOptions _ rs ppu _ fonts) -> do
      res <- deltaRenderOpenGL win ppu fonts rs d w

      mojaveHack win

      return res

  cleanup (OpenGLBackend win _ _) = liftIO $ destroyWindow win

  cycleRenderingOption (OpenGLBackend _ _ mRO) fontType fontSize =
    liftIO $ readMVar mRO >>= \(RenderingOptions catalog _ _ _ fonts) -> do
      let catalog' = updateVariations fontType fontSize catalog
          (FontVariation ppu margin) = getCurrentVariation catalog'
          (content, name) = getFont (getCurrentFont catalog') catalog'
          mkNewFonts = createFonts $ \_ ->
            withFont content name . createFont ppu margin
      mkNewFonts >>= onNewFonts mRO fonts margin ppu catalog'

  ppuDelta (OpenGLBackend _ _ mRO) ppuD =
    liftIO $ updateFont mRO (Just ppuD) Nothing

  fontMarginDelta (OpenGLBackend _ _ mRO) fmD =
    liftIO $ updateFont mRO Nothing (Just fmD)

  getDiscreteSize (OpenGLBackend win _ ro) =
    liftIO $ readMVar ro >>= \(RenderingOptions _ _ (Size ppuH ppuW) _ _) ->
      getFramebufferSize win >>= \(Size h w) ->
        return $ Just $ Size
          (quotCeil h $ fromIntegral ppuH)
          (quotCeil w $ fromIntegral ppuW)
  {-# INLINABLE render #-}
  {-# INLINABLE cleanup #-}
  {-# INLINABLE getDiscreteSize #-}

updateFont :: MVar RenderingOptions -> Maybe PPU -> Maybe FontMargin -> IO (Either String ())
updateFont r mayDeltaPPU mayDeltaMargin =
  readMVar r >>= \(RenderingOptions a _ oldPPU oldMargin fonts) -> do
    let margin = maybe oldMargin (oldMargin +) mayDeltaMargin
        ppu = maybe oldPPU (sumSizes oldPPU) mayDeltaPPU
    createFonts
      -- recycle current fonts:
      (flip (createFont ppu margin) . getFTGLFont . flip lookupFont fonts)
      >>= onNewFonts r fonts margin ppu a

onNewFonts :: MVar RenderingOptions -> Fonts -> FontMargin -> PPU -> FontsVariations -> Either String Fonts -> IO (Either String ())
onNewFonts r fonts margin ppu fontVars = either
  (\e -> do
    putStrLn e
    -- restore the previous sizes
    applySizes fonts
    return $ Left e)
  (\newFonts -> do
      swapMVar r (RenderingOptions fontVars AllFont ppu margin newFonts)
          >>= \(RenderingOptions _ _ _ _ oldFonts) -> do
            print (ppu,margin,newFonts)
            destroyUnusedFonts oldFonts newFonts
      return $ Right ())

-- creates a font, and deletes it in case of error.
withFont :: (MonadIO m)
         => ByteString
         -> String
         -> (FTGL.Font -> m (Either String a))
         -> m (Either String a)
withFont b s f =
  liftIO (loadFont b s) >>= \font -> f font >>= either
    (\err -> liftIO (FTGL.destroyFont font) >> return (Left err))
    (return . Right)

{-# INLINABLE quotCeil #-}
quotCeil :: (Integral a) => a -> a -> a
quotCeil x y =
  let (q,r) = quotRem x y
  in if r == 0
       then q
       else q + 1

instance PlayerInput OpenGLBackend where
  programShouldEnd (OpenGLBackend win _ _) = liftIO $ GLFW.windowShouldClose win
  plaformQueue (OpenGLBackend _ q _) = q
  pollKeys _ = liftIO GLFW.pollEvents
  waitKeys _ = liftIO GLFW.waitEvents
  stopWaitKeys _ = liftIO GLFW.postEmptyEvent
  waitKeysTimeout _ = liftIO . GLFW.waitEventsTimeout . unsafeToSecs
  queueType _ = ManualFeed

  {-# INLINABLE programShouldEnd #-}
  {-# INLINABLE plaformQueue #-}
  {-# INLINABLE stopWaitKeys #-}
  {-# INLINABLE pollKeys #-}
  {-# INLINABLE waitKeys #-}
  {-# INLINABLE waitKeysTimeout #-}
  {-# INLINABLE queueType #-}

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
  GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback q

  -- TODO when ppu changes, use 'floorToPPUMultiple', and glfwSetWindowSize
  GLFW.pollEvents -- this is necessary to show the window
  let margin = FontMargin 0
      fv = mkFontsVariations
      (content,name) = getFont 0 fv
  createFonts (\_ -> withFont content name . createFont ppu margin) >>= either
    (return . Left)
    (\fonts -> Right . OpenGLBackend win q <$>
        newMVar (RenderingOptions fv AllFont ppu margin fonts))

createWindow :: String -> Maybe Size -> IO GLFW.Window
createWindow title s = do
  (mon, Size (Length height) (Length width)) <- maybe
    ( -- full screen mode
      GLFW.getPrimaryMonitor >>= maybe
        (return (Nothing, Size 600 1200))
        (\mon -> GLFW.getVideoMode mon >>= maybe
          (return (Nothing, Size 600 1200))
          (\mode -> do
              GLFW.windowHint $ GLFW.WindowHint'RedBits     $ Just $ GLFW.videoModeRedBits mode
              GLFW.windowHint $ GLFW.WindowHint'GreenBits   $ Just $ GLFW.videoModeGreenBits mode
              GLFW.windowHint $ GLFW.WindowHint'BlueBits    $ Just $ GLFW.videoModeBlueBits mode
              GLFW.windowHint $ GLFW.WindowHint'RefreshRate $ Just $ GLFW.videoModeRefreshRate mode
              return (Just mon
                    , Size (fromIntegral $ GLFW.videoModeHeight mode)
                           (fromIntegral $ GLFW.videoModeWidth mode))
              )))
    (\s' -> return (Nothing, s'))
    s
  -- Single buffering goes well with delta-rendering, hence we use single buffering.
  GLFW.windowHint $ GLFW.WindowHint'DoubleBuffer False
  win <- fromMaybe (error $ "could not create a GLFW window of size " ++ show s) <$>
    GLFW.createWindow width height title mon Nothing

  GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
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

getFramebufferSize :: GLFW.Window -> IO Size
getFramebufferSize win =
  GLFW.getFramebufferSize win >>= \(w,h) ->
    return $ Size (fromIntegral h) (fromIntegral w)

deltaRenderOpenGL :: GLFW.Window
                  -> PPU
                  -> Fonts
                  -> RenderingStyle
                  -> Delta
                  -> Dim Width
                  -> IO (Time Duration System, Time Duration System)
deltaRenderOpenGL win ppu fonts rs (Delta delta) w = do
  t1 <- getSystemTime
  renderDelta win ppu fonts rs delta w -- TODO optimize : this is slow when we draw to the whole screen
  t2 <- getSystemTime
  -- To make sure all commands are visible on screen after this call, since we are
  -- in single-buffer mode, we use glFinish:
  GL.finish
  t3 <- getSystemTime
  return (t1...t2,t2...t3)

renderDelta :: GLFW.Window
            -> PPU
            -> Fonts
            -> RenderingStyle
            -> Dyn.IOVector Cell
            -> Dim Width
            -> IO ()
renderDelta win ppu fonts rs delta' w = do
  fbSz@(Size he wi) <- getFramebufferSize win
  GL.viewport GL.$= (GL.Position 0 0,GL.Size (fromIntegral wi) (fromIntegral he))
  sz <- Dyn.length delta'
  delta <- Dyn.accessUnderlying delta'
      -- We pass the underlying vector, and the size instead of the dynamicVector
  let renderDelta' :: Dim BufferIndex -> IO ()
      renderDelta' index
       | fromIntegral sz == index = return ()
       | otherwise = do
          c <- unsafeRead delta $ fromIntegral index
          let (bg, fg, idx, glyph) = expandIndexed c
              (char,fontIndex) = decodeGlyph glyph
              font = lookupFont fontIndex fonts
              (x,y) = xyFromIndex w idx
          draw ppu fbSz font rs x y char bg fg
          renderDelta' $ succ index
  void $ renderDelta' 0

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
   (UnitColor bR bG bB) = color8ToUnitRGB bg
   (UnitColor fR fG fB) = color8ToUnitRGB fg


renderChar :: Font -> PPU -> Size -> Dim Col -> Dim Row -> Char -> GL.Color3 GL.GLfloat -> IO ()
renderChar (Font font _ (Vec2 offsetCol offsetRow)) (Size ppuH ppuW) (Size winHeight winWidth) c r char color = do
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
