{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Imj.Graphics.Render.Delta.Backend.OpenGL
    ( newOpenGLBackend
    , OpenGLBackend
    ) where

import           Imj.Prelude hiding((<>))
import           Prelude(putStrLn)

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

pixelPerUnit :: Int
pixelPerUnit = 4

newtype OpenGLBackend = OpenGLBackend GLFW.Window

instance DeltaRenderBackend OpenGLBackend where
    render :: OpenGLBackend -> Delta -> Dim Width -> IO ()
    render (OpenGLBackend win) = deltaRenderOpenGL win

    cleanup :: OpenGLBackend -> IO ()
    cleanup (OpenGLBackend win) = destroyWindow win

    getDiscreteSize :: OpenGLBackend -> IO (Maybe Size)
    getDiscreteSize _ =
      return $ Just $ Size (fromIntegral $ quot winHeight pixelPerUnit)
                           (fromIntegral $ quot winWidth pixelPerUnit)

winWidth :: Int
winHeight :: Int
winWidth = 640
winHeight = 480

newOpenGLBackend :: String -> IO OpenGLBackend
newOpenGLBackend title = do
  let simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  unless r $ error "could not initialize GLFW"

  OpenGLBackend <$> createWindow winWidth winHeight title

createWindow :: Int -> Int -> String -> IO GLFW.Window
createWindow width height title = do
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
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
deltaRenderOpenGL win (Delta delta) w = do
  putStrLn "render"
  renderDelta delta w
  putStrLn "rendered"
  -- Single buffering goes well with delta-rendering, hence we use single buffering.
  GL.finish -- make sure all commands are visible on screen after this call
  GLFW.pollEvents -- necessary so that the window is shown.

renderDelta :: Dyn.IOVector Cell
            -> Dim Width
            -> IO ()
renderDelta delta' w = do
  sz <- Dyn.length delta'
  putStrLn $ show sz
  delta <- Dyn.accessUnderlying delta'
      -- We pass the underlying vector, and the size instead of the dynamicVector
  let renderDelta' :: Dim BufferIndex -> IO ()
      renderDelta' index
       | fromIntegral sz == index = return ()
       | otherwise = do
          putStrLn $ show index
          c <- read delta $ fromIntegral index
          let (bg, fg, idx, char) = expandIndexed c
              (x,y) = xyFromIndex w idx
          draw x y char bg fg
          renderDelta' $ succ index
  void (renderDelta' 0)
  putStrLn $ show w

draw :: Dim Col -> Dim Row -> Char -> Color8 Background -> Color8 Foreground -> IO ()
draw c' r' _char _bg _fg = do
  let color3f = GL.color $ GL.Color3 r g (b :: GL.GLfloat)
      vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GL.GLfloat)
      (r1,r2) = (unitRow r', unitRow (succ r'))
      (c1,c2) = (unitCol c', unitCol (succ c'))
      totalH = fromIntegral $ quot winHeight pixelPerUnit
      unitRow x = -1 + unit winHeight * (fromIntegral (totalH-x) - 0.5)
      unitCol x = -1 + unit winWidth * (fromIntegral x + 0.5)

      unit x = 2 * recip (fromIntegral $ quot x pixelPerUnit)

      (r,g,b) = color8ToUnitRGB _fg
  GL.renderPrimitive GL.TriangleFan $ do
    color3f
    vertex3f c2 r1 0
    vertex3f c2 r2 0
    vertex3f c1 r2 0
    vertex3f c1 r1 0
