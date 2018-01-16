{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.Render.Delta.Backend.OpenGL
    ( newOpenGLBackend
    , OpenGLBackend
    , windowCloseCallback -- for doc
    ) where

import           Imj.Prelude hiding((<>))
import           Prelude(putStrLn, length)
import           Control.Concurrent.STM(TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import           Data.Char(ord, chr, isHexDigit, digitToInt)
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
import           Imj.Timing

pixelPerUnit :: Int
pixelPerUnit = 4

data EventKey = EventKey !Key !Int !GLFW.KeyState !GLFW.ModifierKeys

keyCallback :: TQueue EventKey -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc _ k sc ka mk =
  atomically $ writeTQueue tc $ EventKey
    (glfwKeyToKey k) sc ka mk

windowCloseCallback :: TQueue EventKey -> GLFW.Window -> IO ()
windowCloseCallback keyQueue _ =
  atomically $ writeTQueue keyQueue $ EventKey
    StopProgram 0 GLFW.KeyState'Pressed $ GLFW.ModifierKeys False False False False

data OpenGLBackend = OpenGLBackend {
    _glfwWin :: !GLFW.Window
  , _glfwKeyEvts :: !(TQueue EventKey)
}

instance DeltaRenderBackend OpenGLBackend where
    render :: OpenGLBackend -> Delta -> Dim Width -> IO ()
    render (OpenGLBackend win _) = deltaRenderOpenGL win

    cleanup :: OpenGLBackend -> IO ()
    cleanup (OpenGLBackend win _) = destroyWindow win

    getDiscreteSize :: OpenGLBackend -> IO (Maybe Size)
    getDiscreteSize _ =
      return $ Just $ Size (fromIntegral $ quot winHeight pixelPerUnit)
                           (fromIntegral $ quot winWidth pixelPerUnit)

-- | First, we try to read from the queue. Then, we 'GLFW.pollEvents',
-- 'GLFW.waitEvents' or 'GLFW.waitEventsTimeout' (depending on the function)
-- and try to read again from the queue.
--
-- If no key press was found, in 'getKey' we recurse.
instance PlayerInput OpenGLBackend where
  getKey (OpenGLBackend _ keyQueue) = do
    let tryReadQueue = liftIO $ tryGetFirstKeyPress keyQueue
        fillQueue = liftIO GLFW.waitEvents
        readQueue =
          tryReadQueue >>= \case
            Just x  -> return x
            Nothing -> fillQueue >> readQueue
    readQueue

  getKeyTimeout b@(OpenGLBackend _ keyQueue) callerTime allowedMicros = do
    let tryReadQueue = liftIO $ tryGetFirstKeyPress keyQueue
    -- Note that 'GLFW.waitEventsTimeout' returns when /any/ event occured. If
    -- the event is not a keypress, we need to recurse and adapt the timeout accordingly.
        tryFillQueue = liftIO $ GLFW.waitEventsTimeout (fromIntegral allowedMicros / 1000000)
    tryReadQueue >>= maybe
      (tryFillQueue >> tryReadQueue >>= \case
        Just k -> return $ Just k
        Nothing -> do
          -- Either there was nothing in the queue (hence, we hit the timeout
          -- or the glfw event was not a key event) or there was a key repeat or key release
          -- event in the queue.
          now <- liftIO getSystemTime
          let elapsedMicros = diffTimeSecToMicros $ diffSystemTime now callerTime
          if elapsedMicros < allowedMicros
            then
              getKeyTimeout b now (allowedMicros - elapsedMicros)
            else
              return Nothing)
      (return . Just)

  tryGetKey (OpenGLBackend _ keyQueue) = do
    let tryReadQueue = liftIO $ tryGetFirstKeyPress keyQueue
        tryFillQueue = liftIO GLFW.pollEvents
    tryReadQueue >>= maybe
      (tryFillQueue >> tryReadQueue)
      (return . Just)

  programShouldEnd (OpenGLBackend win _) =
    liftIO $ GLFW.windowShouldClose win

  {-# INLINABLE tryGetKey #-}
  {-# INLINABLE getKey #-}
  {-# INLINABLE getKeyTimeout #-}
  {-# INLINABLE programShouldEnd #-}

{-# INLINABLE tryGetFirstKeyPress #-}
tryGetFirstKeyPress :: TQueue EventKey -> IO (Maybe Key)
tryGetFirstKeyPress keyQueue = do
  (liftIO $ atomically $ tryReadTQueue keyQueue) >>= \case
    Nothing -> return Nothing
    Just (EventKey key _ GLFW.KeyState'Pressed _) -> return $ Just key
    Just _ -> tryGetFirstKeyPress keyQueue


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

winWidth :: Int
winHeight :: Int
winWidth = 800
winHeight = 480

newOpenGLBackend :: String -> IO OpenGLBackend
newOpenGLBackend title = do
  let simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  unless r $ error "could not initialize GLFW"

  keyEventsChan <- newTQueueIO :: IO (TQueue EventKey)
  win <- createWindow winWidth winHeight title
  GLFW.setKeyCallback win $ Just $ keyCallback keyEventsChan
  GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback keyEventsChan
  return $ OpenGLBackend win keyEventsChan

createWindow :: Int -> Int -> String -> IO GLFW.Window
createWindow width height title = do
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  -- Single buffering goes well with delta-rendering, hence we use single buffering.
  GLFW.windowHint $ GLFW.WindowHint'DoubleBuffer False
  m <- GLFW.createWindow width height title Nothing Nothing
  let win = fromMaybe (error "could not create GLFW window") m
  GLFW.makeContextCurrent (Just win)

  GL.clearColor GL.$= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

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

deltaRenderOpenGL :: GLFW.Window -> Delta -> Dim Width -> IO ()
deltaRenderOpenGL _ (Delta delta) w = do
  renderDelta delta w
  -- To make sure all commands are visible on screen after this call, since we are
  -- in single-buffer mode, we use glFinish:
  GL.finish

renderDelta :: Dyn.IOVector Cell
            -> Dim Width
            -> IO ()
renderDelta delta' w = do
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
          draw x y char bg fg
          renderDelta' $ succ index
  void (renderDelta' 0)

draw :: Dim Col -> Dim Row -> Char -> Color8 Background -> Color8 Foreground -> IO ()
draw col row char bg fg
  | isHexDigit char = do
    let n = digitToInt char
        (d,d') = quotRem n 8
        (c,c') = quotRem d' 4
        (b,a) = quotRem c' 2
        colorOn  = GL.Color3 1 1 0       -- yellow
        colorOff = GL.Color3 0.4 0.4 0.4 -- grey
        bits = [a,b,c,d]
        locations = [(0.5,0.5),(1.5,0.5),(1.5,1.5),(0.5,1.5)]
    mapM_
      (\((dx, dy), bit) -> do
        let color
              | bit == 0 = colorOff
              | otherwise = colorOn
        drawSquare (quot pixelPerUnit 2) (2 * fromIntegral col + dx)
                                         (2 * fromIntegral row + dy) color)
      $ zip locations bits
  | otherwise = do
  let (r,g,b) = if char == ' '
                  then color8ToUnitRGB bg
                  else color8ToUnitRGB fg
  drawSquare pixelPerUnit (fromIntegral col) (fromIntegral row) $ GL.Color3 r g (b :: GL.GLfloat)


drawSquare :: Int -> Float -> Float -> GL.Color3 GL.GLfloat -> IO ()
drawSquare ppu c' r' color = do
  let vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GL.GLfloat)
      unit x = 2 * recip (fromIntegral $ quot x ppu)
      -- the +/- 0.5 are to place at pixel centers.
      totalH = fromIntegral $ quot winHeight ppu
      unitRow x = -1 + unit winHeight * ((totalH-x) - 0.5)
      unitCol x = -1 + unit winWidth * (x + 0.5)

      (r1,r2) = (unitRow r', unitRow (1 + r'))
      (c1,c2) = (unitCol c', unitCol (1 + c'))
  GL.renderPrimitive GL.TriangleFan $ do
    GL.color color
    vertex3f c2 r1 0
    vertex3f c2 r2 0
    vertex3f c1 r2 0
    vertex3f c1 r1 0
