import           System.Console.ANSI(clearScreen)
import           Control.Monad.Reader(runReaderT)

import           Imj.Graphics.Render
import           Imj.Graphics.Render.Naive

import           Test.Imj.Bresenham2
import           Test.Imj.Bresenham3
import           Test.Imj.Color
--import           Test.Imj.Ease
import           Test.Imj.Interpolation
import           Test.Imj.InterpolatedColorString
import           Test.Imj.StdoutBuffer
import           Test.Imj.RectArea
import           Test.Imj.Resample
import           Test.Imj.Segment
import           Test.Imj.Timing
import           Test.Imj.Util
import           Test.Imj.Vector
import           Test.Imj.ZigZag

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testBres2 >>= print
  testBres3 >>= print
  testRectArea >>= print
  testResample >>= print
  testTiming >>= print
  testSegment >>= print
  testVector >>= print
  testZigZag >>= print
  testInterpolation
  testUtils
  testColor

  clearScreen -- to not overwrite current terminal content.
  _ <- runReaderT (testICS >>
                   renderToScreen
                   ) NaiveDraw
  --testEase
  testMutableBytestring
  testLastAbove
  return ()
