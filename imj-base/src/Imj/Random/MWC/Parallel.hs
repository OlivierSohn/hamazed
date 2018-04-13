{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Random.MWC.Parallel
        ( mkOneGenPerCapability
        ) where

import           Imj.Prelude

import           Control.Concurrent(getNumCapabilities)
import           Data.List.NonEmpty (NonEmpty(..), fromList)
import           System.Random.MWC(GenIO, withSystemRandom, asGenIO)

-- | Creates one RNG per capability. Each RNG has a unique seed.
mkOneGenPerCapability :: IO (NonEmpty GenIO)
mkOneGenPerCapability = do
  nWorkers <- max 1 <$> getNumCapabilities
  fromList <$> go nWorkers []
 where
  go 0 l = return l
  go nWorkers l =
  -- withSystemRandom seeds a PRNG with data from the system's fast source of pseudo-random numbers.
  -- The generator should be used from a single thread.
    withSystemRandom . asGenIO $ go (nWorkers-1) . flip (:) l
