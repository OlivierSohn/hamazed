import System.Console.ANSI(clearScreen)

import Control.Monad.Reader(runReaderT)

import Draw

import Test.Bresenham3
import Test.InterpolatedColorString
import Test.Interpolation(testInterpolation)
import Test.MockDraw

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testInterpolation
  testBres3 >>= print

  clearScreen
  runReaderT (testICS >>
              renderDrawing
              ) (MockDraw 2)
