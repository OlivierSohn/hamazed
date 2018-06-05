module Test.Imj.ZigZag
         ( testZigZag
         ) where

import           Control.Monad(when)

import           Imj.Util

-- | returns 1 on success, else errors
testZigZag :: IO Bool
testZigZag = do
  let res = map (zigzag 3 7) [0..15]
  when (res /= [3,4,5,6,7,6,5,4,3,4,5,6,7,6,5,4::Int]) $ error $ show res
  return True
