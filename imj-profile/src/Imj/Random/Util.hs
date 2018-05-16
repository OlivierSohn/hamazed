{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Random.Util
       ( withRandom
       , randomRsIO
       , withRandomL
       ) where

import           Imj.Prelude hiding(range)
import           Control.Arrow
import           Data.List(take)
import           Data.Word(Word32)
import           System.Random

withRandom :: (IO Word32 -> IO a) -> IO (String,a)
withRandom act = do
  setStdGen $ mkStdGen seed -- make test deterministic

  (,) "random" <$> act randomIO'
 where
  seed = 0

withRandomL :: ((Int -> IO [Word32]) -> IO a) -> IO (String,a)
withRandomL act = do
  setStdGen $ mkStdGen seed -- make test deterministic

  (,) "randomL" <$> act (\n -> take n <$> randomsIO')
 where
  seed = 0

randomIO' :: Random a => IO a
randomIO' = getStdRandom random
randomsIO' :: Random a => IO [a]
randomsIO' = getStdRandom $ split >>> first randoms

randomRsIO :: Random a => (a,a) -> IO [a]
randomRsIO range = getStdRandom $ split >>> first (randomRs range)
