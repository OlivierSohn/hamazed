import Control.Monad.Reader(runReaderT)

import Env

import Render.Delta(restoreConsole)
import Render.Draw

import Test.Interpolation(testInterpolation)
import Test.InterpolatedColorString
import Test.Bresenham3
import Test.Stdout
import Test.Reader
import Test.Rendering

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testInterpolation
  testBres3 >>= print
  testReader

  env <- createEnv
  runReaderT (testSpace >>
              testICS >>
              flush
              ) env
  restoreConsole

  --testStdout
