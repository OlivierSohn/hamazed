{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Graphics.Render.Delta.Backend.OpenGL
    ( newOpenGLBackend
    , OpenGLBackend
    , windowCloseCallback -- for doc
    ) where

import           Imj.Prelude hiding((<>))
import           Prelude(putStrLn, length)

import           Control.Concurrent.MVar.Strict(MVar, newMVar, swapMVar, readMVar)
import           Control.Concurrent.STM
                  (TQueue,
                  atomically, newTQueueIO, tryReadTQueue, unGetTQueue, writeTQueue)
import           Control.DeepSeq(NFData)
import           Data.Char(ord, chr, isHexDigit, digitToInt)
import           Data.Maybe(isJust)
import           Foreign.Ptr(nullPtr)
import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           Data.Vector.Unboxed.Mutable(read)
import qualified Imj.Data.Vector.Unboxed.Mutable.Dynamic as Dyn
                        (IOVector, accessUnderlying, length)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Env
import           Imj.Graphics.Render.Delta.Internal.Types
import           Imj.Graphics.Render.Delta.Cell
import           Imj.Input.Types

data EventKey = EventKey !Key !Int !GLFW.KeyState !GLFW.ModifierKeys

keyCallback :: TQueue EventKey -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc _ k sc ka mk =
  atomically $ writeTQueue tc $ EventKey
    (glfwKeyToKey k) sc ka mk

windowCloseCallback :: TQueue EventKey -> GLFW.Window -> IO ()
windowCloseCallback keyQueue _ =
  atomically $ writeTQueue keyQueue $ mkKeyPress StopProgram

{-# INLINE mkKeyPress #-}
mkKeyPress :: Key -> EventKey
mkKeyPress k =
  EventKey k 0 GLFW.KeyState'Pressed $ GLFW.ModifierKeys False False False False

data OpenGLBackend = OpenGLBackend {
    _glfwWin :: !GLFW.Window
  , _glfwKeyEvts :: !(TQueue EventKey)
  , _ppu :: !Int
  -- ^ Number of pixels per discrete unit length. Keep it even for best results.
  , _windowSize :: !Size
  -- ^ In number of pixels.
  , _fonts :: !(MVar RenderingOptions)
  -- ^ Mutable rendering options
}

data RenderingOptions = RenderingOptions !Int !RenderingStyle !FTGL.Font
  deriving(Generic, NFData, Show)

instance PrettyVal RenderingOptions where
  prettyVal opt = prettyVal ("RenderingOptions:", show opt)

data RenderingStyle = AllFont
                    | SquareNums
                    deriving(Generic, NFData, Eq, Show, PrettyVal)

instance DeltaRenderBackend OpenGLBackend where
    render (OpenGLBackend win _ ppu size mFont) d w =
      readMVar mFont >>= \(RenderingOptions _ rs font) ->
        deltaRenderOpenGL win ppu size font rs d w

    cleanup (OpenGLBackend win _ _ _ _) = destroyWindow win

    cycleRenderingOption (OpenGLBackend _ _ ppu _ mRO) =
      void( readMVar mRO >>= cycleRenderingOptions ppu >>= swapMVar mRO )

    getDiscreteSize (OpenGLBackend _ _ pixelPerUnit (Size h w) _) =
      return $ Just $ Size (fromIntegral $ quot (fromIntegral h) pixelPerUnit)
                           (fromIntegral $ quot (fromIntegral w) pixelPerUnit)
    {-# INLINABLE render #-}
    {-# INLINABLE cleanup #-}
    {-# INLINABLE getDiscreteSize #-}

-- | First, we try to read from the queue. Then, we 'GLFW.pollEvents',
-- 'GLFW.waitEvents' or 'GLFW.waitEventsTimeout' (depending on the function)
-- and try to read again from the queue.
--
-- If no key press was found, in 'getKey' we recurse.
instance PlayerInput OpenGLBackend where
  getKey (OpenGLBackend _ keyQueue _ _ _) = do
    let tryReadQueue = liftIO $ tryReadFirstKeyPress keyQueue
        fillQueue = liftIO GLFW.waitEvents
        readQueue =
          tryReadQueue >>= \case
            Just x  -> return x
            Nothing -> fillQueue >> readQueue
    readQueue

  getKeyBefore b@(OpenGLBackend _ keyQueue _ _ _) t = do
    let tryReadQueue = liftIO $ tryReadFirstKeyPress keyQueue
        -- Note that 'GLFW.waitEventsTimeout' returns when /any/ event occured. If
        -- the event is not a keypress, we need to recurse and adapt the timeout accordingly.
        tryFillQueue x = liftIO $ GLFW.waitEventsTimeout $ unsafeToSecs x

    liftIO (getDurationFromNowTo t) >>= \allowed -> do
      tryReadQueue >>= maybe
        (if strictlyNegative allowed
          then
            return Nothing
          else
            tryFillQueue allowed >> tryReadQueue >>= \case
              Just k -> return $ Just k
              Nothing -> do
                -- Either there was nothing in the queue (hence, we hit the timeout
                -- or the glfw event was not a key event) or there was a key repeat or key release
                -- event in the queue.
                getKeyBefore b t)
        (return . Just)

  tryGetKey (OpenGLBackend _ keyQueue _ _ _) = do
    let tryReadQueue = liftIO $ tryReadFirstKeyPress keyQueue
        tryFillQueue = liftIO GLFW.pollEvents
    tryReadQueue >>= maybe
      (tryFillQueue >> tryReadQueue)
      (return . Just)

  unGetKey (OpenGLBackend _ keyQueue _ _ _) k =
    liftIO $ atomically $ unGetTQueue keyQueue $ mkKeyPress k

  -- we don't return the peeked value, because we are unable to peek stdin and want a common interface.
  someInputIsAvailable (OpenGLBackend _ keyQueue _ _ _) = do
    let tryPeekQueue = liftIO $ tryPeekFirstKeyPress keyQueue
        tryFillQueue = liftIO GLFW.pollEvents
    tryPeekQueue >>= maybe
      (tryFillQueue >> tryPeekQueue >>= return . isJust)
      (const $ return True)

  programShouldEnd (OpenGLBackend win _ _ _ _) =
    liftIO $ GLFW.windowShouldClose win

  {-# INLINABLE tryGetKey #-}
  {-# INLINABLE someInputIsAvailable #-}
  {-# INLINABLE getKey #-}
  {-# INLINABLE getKeyBefore #-}
  {-# INLINABLE programShouldEnd #-}

{-# INLINABLE tryReadFirstKeyPress #-}
tryReadFirstKeyPress :: TQueue EventKey
                     -> IO (Maybe Key)
tryReadFirstKeyPress keyQueue = do
  (liftIO $ atomically $ tryReadTQueue keyQueue) >>= \case
    Nothing -> return Nothing
    Just (EventKey key _ GLFW.KeyState'Pressed _) -> return $ Just key
    Just _ -> tryReadFirstKeyPress keyQueue

{-# INLINABLE tryPeekFirstKeyPress #-}
tryPeekFirstKeyPress :: TQueue EventKey
                     -> IO (Maybe Key)
tryPeekFirstKeyPress keyQueue = do
  (liftIO $ atomically $ tryReadTQueue keyQueue) >>= \case
    Nothing -> return Nothing
    Just e@(EventKey key _ GLFW.KeyState'Pressed _) -> do
      liftIO (atomically $ unGetTQueue keyQueue e)
      return $ Just key
    Just _ -> tryPeekFirstKeyPress keyQueue


glfwKeyToKey :: GLFW.Key -> Key
glfwKeyToKey k
  | k >= GLFW.Key'0 && k <= GLFW.Key'9 = AlphaNum $ chr $ (ord '0') + (length [GLFW.Key'0 .. pred k])
  | k >= GLFW.Key'A && k <= GLFW.Key'Z = AlphaNum $ chr $ (ord 'a') + (length [GLFW.Key'A .. pred k])
glfwKeyToKey GLFW.Key'Space = AlphaNum ' '
glfwKeyToKey GLFW.Key'Escape = Escape
glfwKeyToKey GLFW.Key'Right = Arrow RIGHT
glfwKeyToKey GLFW.Key'Left  = Arrow LEFT
glfwKeyToKey GLFW.Key'Down  = Arrow Down
glfwKeyToKey GLFW.Key'Up    = Arrow Up
glfwKeyToKey _ = Unknown

newOpenGLBackend :: String -> Int -> Size -> IO OpenGLBackend
newOpenGLBackend title ppu size@(Size h w) = do
  let simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
  GLFW.setErrorCallback $ Just simpleErrorCallback

  GLFW.init >>= \case
    False -> error "could not initialize GLFW"
    True -> return ()

  keyEventsChan <- newTQueueIO :: IO (TQueue EventKey)
  win <- createWindow (fromIntegral w) (fromIntegral h) title
  GLFW.setKeyCallback win $ Just $ keyCallback keyEventsChan
  GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback keyEventsChan
  ro <- RenderingOptions 0 AllFont <$> createFont 0 ppu
  OpenGLBackend win keyEventsChan ppu size <$> newMVar ro

cycleRenderingOptions :: Int -> RenderingOptions -> IO RenderingOptions
cycleRenderingOptions ppu (RenderingOptions idx rs font) = do
  let newRo = case rs of
        AllFont -> SquareNums
        SquareNums -> AllFont
      newIdx = succ idx
      (q,r) = quotRem newIdx 2
      l = length fontNames
  RenderingOptions newIdx newRo
    <$> if 0 == r && l > 1
          then createFont (q `mod` l) ppu
          else return font

fontNames :: [String]
fontNames =
  [ "SourceCodePro-Bold" ]

createFont :: Int -> Int -> IO FTGL.Font
createFont idx ppu = do
  let fontName = fontNames!!idx  ++ ".ttf"
  --font <- FTGL.createBitmapFont fontName -- gives brighter colors but the shape of letters is awkward.
  font <- FTGL.createPixmapFont fontName
  -- the creation methods that "don't work" (i.e need to use matrix positionning?)
  --font <- FTGL.createTextureFont fontName
  --font <- FTGL.createBufferFont fontName
  --font <- FTGL.createOutlineFont fontName
  --font <- FTGL.createPolygonFont fontName
  --font <- FTGL.createTextureFont fontName
  --font <- FTGL.createExtrudeFont fontName

  when (font == nullPtr) $ error $ "font not found : " ++ fontName
  _ <- FTGL.setFontFaceSize font ppu 72-- 24 72
  putStrLn $ "using font: " ++ fontName
  return font

createWindow :: Int -> Int -> String -> IO GLFW.Window
createWindow width height title = do
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  -- Single buffering goes well with delta-rendering, hence we use single buffering.
  GLFW.windowHint $ GLFW.WindowHint'DoubleBuffer False
  m <- GLFW.createWindow width height title Nothing Nothing
  let win = fromMaybe (error "could not create GLFW window") m
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
                  -> Int
                  -> Size
                  -> FTGL.Font
                  -> RenderingStyle
                  -> Delta
                  -> Dim Width
                  -> IO (Time Duration System, Time Duration System)
deltaRenderOpenGL _ ppu size font rs (Delta delta) w = do
  t1 <- getSystemTime
  renderDelta ppu size font rs delta w
  t2 <- getSystemTime
  -- To make sure all commands are visible on screen after this call, since we are
  -- in single-buffer mode, we use glFinish:
  GL.finish
  t3 <- getSystemTime
  return (t1...t2,t2...t3)

renderDelta :: Int
            -> Size
            -> FTGL.Font
            -> RenderingStyle
            -> Dyn.IOVector Cell
            -> Dim Width
            -> IO ()
renderDelta ppu size font rs delta' w = do
  sz <- Dyn.length delta'
  delta <- Dyn.accessUnderlying delta'
      -- We pass the underlying vector, and the size instead of the dynamicVector
  let renderDelta' :: Dim BufferIndex -> IO ()
      renderDelta' index
       | fromIntegral sz == index = return ()
       | otherwise = do
          c <- read delta $ fromIntegral index
          let (bg, fg, idx, char) = expandIndexed c
              (x,y) = xyFromIndex w idx
          draw ppu size font rs x y char bg fg
          renderDelta' $ succ index
  void (renderDelta' 0)

draw :: Int
     -> Size
     -> FTGL.Font
     -> RenderingStyle
     -> Dim Col -> Dim Row
     -> Char
     -> Color8 Background -> Color8 Foreground -> IO ()
draw ppu size font rs col row char bg fg
  | rs == SquareNums && isHexDigit char = do
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
        drawSquare (quot ppu 2) size (2 * fromIntegral col + dx)
                                     (2 * fromIntegral row + dy) color)
      $ zip locations bits
  | otherwise = do
      drawSquare ppu size (fromIntegral col) (fromIntegral row) $ GL.Color3 bR bG (bB :: GL.GLfloat)
      renderChar font ppu size col row char $ GL.Color3 fR fG (fB :: GL.GLfloat)
 where
   (bR,bG,bB) = color8ToUnitRGB bg
   (fR,fG,fB) = color8ToUnitRGB fg


renderChar :: FTGL.Font -> Int -> Size -> Dim Col -> Dim Row -> Char -> GL.Color3 GL.GLfloat -> IO ()
renderChar font ppu (Size winHeight winWidth) col row c color = do
  let unit x = 2 * recip (fromIntegral $ quot x ppu)
      totalH = fromIntegral $ quot (fromIntegral winHeight) ppu
      -- with 6,5 there are leftovers on top of pipe, with 4 it's below.
      unitRow x = -1 + (4/fromIntegral winHeight) + unit (fromIntegral winHeight) * (totalH - x)
      unitCol x = -1 + (4/fromIntegral winWidth) + unit (fromIntegral winWidth) * x
  -- The present raster color is locked by the use glRasterPos*()
  -- <https://www.khronos.org/opengl/wiki/Coloring_a_bitmap>
  --
  -- Hence, we set the color /before/ raster pos:
  GL.color color
  GL.rasterPos $ GL.Vertex2 (unitCol (fromIntegral col :: Float) :: GL.GLfloat)
                            (unitRow (fromIntegral (succ row) :: Float))
  FTGL.renderFont font [c] FTGL.Front --FTGL.Side

drawSquare :: Int -> Size -> Float -> Float -> GL.Color3 GL.GLfloat -> IO ()
drawSquare ppu (Size winHeight winWidth) c' r' color = do
  let vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GL.GLfloat)
      unit x = 2 * recip (fromIntegral $ quot x ppu)
      totalH = fromIntegral $ quot (fromIntegral winHeight) ppu
      unitRow x = -1 + unit (fromIntegral winHeight) * (totalH - x)
      unitCol x = -1 + unit (fromIntegral winWidth) * x

      (r1,r2) = (unitRow r', unitRow (1 + r'))
      (c1,c2) = (unitCol c', unitCol (1 + c'))
  GL.renderPrimitive GL.TriangleFan $ do
    GL.color color
    vertex3f c2 r1 0
    vertex3f c2 r2 0
    vertex3f c1 r2 0
    vertex3f c1 r1 0
