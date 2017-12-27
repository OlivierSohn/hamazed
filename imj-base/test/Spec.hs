import           System.Console.ANSI(clearScreen)
import           Control.Monad.Reader(runReaderT)

import           Imj.Draw
import           Imj.Render.Naive

--import           Test.Imj.Stdout
--import           Test.Imj.Ease
import           Test.Imj.Vector
import           Test.Imj.Bresenham3
import           Test.Imj.Timing
import           Test.Imj.Interpolation
import           Test.Imj.InterpolatedColorString

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testBres3 >>= print
  testTiming >>= print
  testVector >>= print
  testInterpolation

  clearScreen -- to not overwrite current terminal content.
  runReaderT (testICS >>
              renderDrawing
              ) (NaiveDraw 2)

  --testStdout
  --testEase
