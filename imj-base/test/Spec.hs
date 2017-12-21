import Test.Bresenham3
import Test.Timing

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testBres3 >>= print
  testTiming >>= print
