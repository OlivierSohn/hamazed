
import Test.Interpolation(testInterpolation)
import Test.InterpolatedColorString
import Test.Bresenham3

main :: IO ()
main = do
  putStrLn "" -- for readablilty
--  testInterpolation
  testICS
--  testBres3 >>= print
