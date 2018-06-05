{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Random.MWC.Util
       ( withNumberedSeeds
       , seedGroups
       , withMWC256
       , randUUID
       ) where

import           Imj.Prelude

import           Control.Concurrent(getNumCapabilities)
import           Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.UUID
import           Data.Vector(fromList)
import           Data.Word(Word32)
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

withMWC256 :: (IO Word32 -> IO a) -> IO (String,a)
withMWC256 act =
  (,) "mwc256" <$>
    withNumberedSeeds (act . uniform . NE.head) (pure seed)
 where
  seed = SeedNumber 0

randUUID :: IO UUID
randUUID = withSystemRandom . asGenIO $ \gen ->
  forM [0 :: Int ..3] (const $ uniform gen) >>= \case
    [w1,w2,w3,w4] -> return $ fromWords w1 w2 w3 w4
    _ -> error "logic"
