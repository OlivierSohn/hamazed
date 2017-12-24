--import Test.Imj.Stdout
--import Test.Imj.Ease
import Test.Imj.Vector
import Test.Imj.Bresenham3
import Test.Imj.Timing
import Test.Imj.Interpolation

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testBres3 >>= print
  testTiming >>= print
  testVector >>= print
  testInterpolation

  --testStdout
  --testEase
