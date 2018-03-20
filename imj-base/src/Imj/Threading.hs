{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Threading
    ( setupCapabilities
    ) where

import           Imj.Prelude

import           GHC.Conc(getNumProcessors)
import           Control.Concurrent( setNumCapabilities )

-- | Sets the number of capabilities to half the number of processors.
setupCapabilities :: IO ()
setupCapabilities = do
  nproc <- getNumProcessors
  let ncap = max 1 $ quot nproc 2
  setNumCapabilities ncap
