{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Imj.Control.Concurrent (
  useOneCapabilityPerPhysicalCore
  ) where

import           Imj.Prelude
import           Prelude(putStrLn, print)
import           GHC.Conc(getNumProcessors)
import           Control.Concurrent(setNumCapabilities)

import Foreign.C.Types 

foreign import ccall "getNumberOfProcessors" c_getNumberOfProcessors :: IO CInt

report :: IO ()
report = c_getNumberOfProcessors >>= print

{- |
Benchmarks on random world creation show that using one capability per physical core is optimal.
-}
useOneCapabilityPerPhysicalCore :: IO ()
useOneCapabilityPerPhysicalCore = do
  report
  getNumProcessors >>= \n -> do
    let nCaps = max 1 $ quot n 2 -- getNumProcessors returns the number of logical core, we assume that half of that are physical cores
    putStrLn $ "Using " ++ show nCaps ++ " capabilities."
    setNumCapabilities nCaps
