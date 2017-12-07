
import Test.Interpolation(testInterpolation)
import Test.InterpolatedColorString
import Test.Bresenham3
import Test.WorldEvolutions

import Animation.Design.Chars
import Data.Char (ord)

main :: IO ()
main = do
  putStrLn "" -- for readablilty
--  testInterpolation
  testICS
--  testBres3 >>= print
--  testWE
  let maxi = maximum $ map (ord . niceChar) [0..100]
  print maxi
