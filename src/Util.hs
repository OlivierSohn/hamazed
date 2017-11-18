
module Util
    ( showListOrSingleton
    , replicateElements
    , randomRsIO
    ) where

import           Imajuscule.Prelude

import           Control.Arrow( (>>>)
                              , first )

import           System.Random( Random(..)
                              , getStdRandom
                              , split )


showListOrSingleton :: Show a => [a] -> String
showListOrSingleton [e] = show e
showListOrSingleton l   = show l

replicateElements :: Int -> [a] -> [a]
replicateElements n = concatMap (replicate n)

randomRsIO :: Random a => (a,a) -> IO [a]
randomRsIO range = getStdRandom $ split >>> first (randomRs range)
