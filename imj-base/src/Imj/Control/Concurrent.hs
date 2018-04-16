{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Control.Concurrent (
  useOneCapabilityPerPhysicalCore
  ) where

import           Imj.Prelude
import           Prelude(putStrLn)
import           GHC.Conc(getNumProcessors)
import           Control.Concurrent(setNumCapabilities)

{- |
Benchmarks on random world creation show that using one capability per physical core is optimal.
-}
useOneCapabilityPerPhysicalCore :: IO ()
useOneCapabilityPerPhysicalCore =
  -- NOTE getNumProcessors resturns the same info as the c function getNumberOfProcessors
  getNumProcessors >>= \n -> do
    let nCaps = max 1 $ quot n 2 -- getNumProcessors returns the number of logical core, we assume that half of that are physical cores
    putStrLn $ "Using " ++ show nCaps ++ " capabilities."
    setNumCapabilities nCaps
