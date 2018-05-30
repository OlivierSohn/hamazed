import           System.Console.ANSI(clearScreen)
import           Control.Monad.Reader(runReaderT)

import           Imj.Graphics.Render
import           Imj.Graphics.Render.Naive

import           Test.Imj.Quantifiable
import           Test.Imj.Bresenham2
import           Test.Imj.Bresenham3
import           Test.Imj.Color
import           Test.Imj.CyclicMatrix
--import           Test.Imj.Ease
import           Test.Imj.FreeType2
import           Test.Imj.Font
import           Test.Imj.Interleave
import           Test.Imj.Interpolation
import           Test.Imj.InterpolatedColorString
import           Test.Imj.Newton
import           Test.Imj.StdoutBuffer
import           Test.Imj.Sums
import           Test.Imj.RectArea
import           Test.Imj.Resample
import           Test.Imj.Segment
import           Test.Imj.Util
import           Test.Imj.Vector
import           Test.Imj.ZigZag

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  testNewton

  testBres2 >>= print
  testBres3 >>= print
  testRectArea >>= print
  testResample >>= print
  testSegment >>= print
  testVector >>= print
  testZigZag >>= print
  testInterpolation
  testColor

  clearScreen
  _ <- flip runReaderT NaiveDraw $ do
    testICS
    renderToScreen

  --testEase
  testMutableBytestring
  testUtils
  testLogBase2
  testLastAbove
  testSums
  testFont
  testFreeType2
  testFreeType2'

  clearScreen
  testInterleaveHalves
  testCyclicMatrix
  testAverageable
  putStrLn ""
  return ()
