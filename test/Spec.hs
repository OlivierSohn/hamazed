import Control.Monad.Reader(runReaderT)

import Env

import Render.Delta(restoreConsole)
import Draw

import Test.Interpolation(testInterpolation)
import Test.InterpolatedColorString
import Test.Bresenham3
import Test.Stdout
import Test.Rendering
import Test.Vector

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testInterpolation
  testVector >>= print
  testBres3 >>= print

  env <- createEnv
  runReaderT (testSpace >>
              testICS >>
              renderDrawing
              ) env
  restoreConsole

  --testStdout
