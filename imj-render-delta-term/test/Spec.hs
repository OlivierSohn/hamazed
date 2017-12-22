--import Test.Imj.Stdout
import Test.Imj.Vector

main :: IO ()
main = do
  putStrLn "" -- for readablilty
  testVector >>= print

  --testStdout
