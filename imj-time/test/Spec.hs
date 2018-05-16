
import           Test.Imj.Timing

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  testTiming >>= print
