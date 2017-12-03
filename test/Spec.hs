
import Test.Interpolation
import Test.InterpolatedColorString
import Test.Bresenham3

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testICS
  testBres3 >>= print
