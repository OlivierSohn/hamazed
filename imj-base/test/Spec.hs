import Test.Imj.Bresenham3
import Test.Imj.Timing

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testBres3 >>= print
  testTiming >>= print
