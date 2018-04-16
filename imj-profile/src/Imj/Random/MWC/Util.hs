{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Random.MWC.Util
       ( withNumberedSeeds
       , seedGroups
       ) where

import           Imj.Prelude

import           Control.Concurrent(getNumCapabilities)
import           Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NE(fromList)
import           Data.Vector(fromList)
import           System.Random.MWC

import           Imj.Profile.Result
import           Imj.Random.MWC.Seeds

withNumberedSeeds :: (NonEmpty GenIO -> IO a) -> NonEmpty SeedNumber -> IO a
withNumberedSeeds act seedNs = do
  -- we use deterministic seeds made from the same source as 'withSystemRandom' (see https://github.com/bos/mwc-random/issues/64)
  gen <- mapM (\(SeedNumber i) -> initialize $ fromList $ deterministicMWCSeeds !! i) seedNs
  --putStr $ unwords [show $ map unSeedNumber $ NE.toList seedNs] ++ " "
  --hFlush stdout
  act gen

seedGroups :: IO [NonEmpty SeedNumber]
seedGroups = do
  nWorkers <- max 1 <$> getNumCapabilities
  return $ map
    (\i ->
      let start = i*nWorkers
      in NE.fromList $ map SeedNumber [start..start+nWorkers-1])
    [0..nGroups-1]
 where
  nGroups = 5
