import Control.Monad.Reader(runReaderT)

import Env

import Render.Delta

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
  testICS
  testBres3 >>= print
  testReader
  ctxt <- newDefaultContext
  runReaderT testSpace $ Env $ mkRenderFunctions ctxt
  --testStdout
