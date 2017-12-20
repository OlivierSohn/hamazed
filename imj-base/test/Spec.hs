import Test.Bresenham3


main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testBres3 >>= print
