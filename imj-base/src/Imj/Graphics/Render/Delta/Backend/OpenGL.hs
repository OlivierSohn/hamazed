{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.Render.Delta.Backend.OpenGL
    ( newOpenGLBackend
    , OpenGLBackend
    , windowCloseCallback -- for doc
    ) where

import           Imj.Prelude hiding((<>))
import           Prelude(putStrLn)
import           Control.Concurrent.STM(TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
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
glfwKeyToKey GLFW.Key'0 = AlphaNum '0'
glfwKeyToKey GLFW.Key'1 = AlphaNum '1'
glfwKeyToKey GLFW.Key'2 = AlphaNum '2'
glfwKeyToKey GLFW.Key'3 = AlphaNum '3'
glfwKeyToKey GLFW.Key'4 = AlphaNum '4'
glfwKeyToKey GLFW.Key'5 = AlphaNum '5'
glfwKeyToKey GLFW.Key'6 = AlphaNum '6'
glfwKeyToKey GLFW.Key'7 = AlphaNum '7'
glfwKeyToKey GLFW.Key'8 = AlphaNum '8'
glfwKeyToKey GLFW.Key'9 = AlphaNum '9'
glfwKeyToKey GLFW.Key'Space = AlphaNum ' '
glfwKeyToKey GLFW.Key'A = AlphaNum 'a'
glfwKeyToKey GLFW.Key'B = AlphaNum 'b'
glfwKeyToKey GLFW.Key'C = AlphaNum 'c'
glfwKeyToKey GLFW.Key'D = AlphaNum 'd'
glfwKeyToKey GLFW.Key'E = AlphaNum 'e'
glfwKeyToKey GLFW.Key'F = AlphaNum 'f'
glfwKeyToKey GLFW.Key'G = AlphaNum 'g'
glfwKeyToKey GLFW.Key'H = AlphaNum 'h'
glfwKeyToKey GLFW.Key'I = AlphaNum 'i'
glfwKeyToKey GLFW.Key'J = AlphaNum 'j'
glfwKeyToKey GLFW.Key'K = AlphaNum 'k'
glfwKeyToKey GLFW.Key'L = AlphaNum 'l'
glfwKeyToKey GLFW.Key'M = AlphaNum 'm'
glfwKeyToKey GLFW.Key'N = AlphaNum 'n'
glfwKeyToKey GLFW.Key'O = AlphaNum 'o'
glfwKeyToKey GLFW.Key'P = AlphaNum 'p'
glfwKeyToKey GLFW.Key'Q = AlphaNum 'q'
glfwKeyToKey GLFW.Key'R = AlphaNum 'r'
glfwKeyToKey GLFW.Key'S = AlphaNum 's'
glfwKeyToKey GLFW.Key'T = AlphaNum 't'
glfwKeyToKey GLFW.Key'U = AlphaNum 'u'
glfwKeyToKey GLFW.Key'V = AlphaNum 'v'
glfwKeyToKey GLFW.Key'W = AlphaNum 'w'
glfwKeyToKey GLFW.Key'X = AlphaNum 'x'
glfwKeyToKey GLFW.Key'Y = AlphaNum 'y'
glfwKeyToKey GLFW.Key'Z = AlphaNum 'z'
glfwKeyToKey GLFW.Key'Right = Arrow RIGHT
glfwKeyToKey GLFW.Key'Left  = Arrow LEFT
glfwKeyToKey GLFW.Key'Down  = Arrow Down
glfwKeyToKey GLFW.Key'Up    = Arrow Up
glfwKeyToKey GLFW.Key'Escape = Escape
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
draw c' r' char bg fg = do
  let color3f = GL.color $ GL.Color3 r g (b :: GL.GLfloat)
      vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GL.GLfloat)
      (r1,r2) = (unitRow r', unitRow (succ r'))
      (c1,c2) = (unitCol c', unitCol (succ c'))
      totalH = fromIntegral $ quot winHeight pixelPerUnit

      -- the +/- 0.5 are to place at pixel centers.
      unitRow x = -1 + unit winHeight * (fromIntegral (totalH-x) - 0.5)
      unitCol x = -1 + unit winWidth * (fromIntegral x + 0.5)

      unit x = 2 * recip (fromIntegral $ quot x pixelPerUnit)

      (r,g,b) = if char == ' '
                  then color8ToUnitRGB bg
                  else color8ToUnitRGB fg
  GL.renderPrimitive GL.TriangleFan $ do
    color3f
    vertex3f c2 r1 0
    vertex3f c2 r2 0
    vertex3f c1 r2 0
    vertex3f c1 r1 0
