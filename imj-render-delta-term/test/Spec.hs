import Test.Stdout
import Test.Vector

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testVector >>= print

  --testStdout
