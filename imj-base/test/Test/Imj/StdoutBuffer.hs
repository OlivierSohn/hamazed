{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
module Test.Imj.StdoutBuffer
    ( testMutableBytestring
    , flush
    ) where

import           Imj.Prelude

import           GHC.IO.Encoding(setLocaleEncoding)
import           System.IO(localeEncoding, hGetEncoding, print, stdout, putStrLn, utf8)

import           Imj.Data.StdoutBuffer

-- | prototype for delta rendering optimizationm using a /fixed/ size buffer.
-- When we are near the end, flush using putStr.
testMutableBytestring :: IO ()
testMutableBytestring = do
  print localeEncoding
  hGetEncoding stdout >>= print
  print "Setting the locale to utf8."
  setLocaleEncoding utf8
  hGetEncoding stdout >>= print

  buf <- mkStdoutBuffer 5
  addStr "1" buf
  addStr "2" buf
  addStr "3" buf
  addStr "4" buf
  addStr "5" buf
  addStr "6" buf
  addStr "7" buf
  addStr "8" buf
  flush buf -- if we remove this call, only 12345 is printed.
  putStrLn ""
