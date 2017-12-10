module Test.Stdout where

import System.IO

testStdout = do
  hShow stdout >>= print
  hGetBuffering stdout >>= print
  hGetEncoding stdout >>= print
