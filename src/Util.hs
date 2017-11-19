{-# LANGUAGE NoImplicitPrelude #-}

module Util
    ( showListOrSingleton
    , replicateElements
    , randomRsIO
    ) where

import           Imajuscule.Prelude

import           Data.Text(Text, pack)

import           Control.Arrow( (>>>)
                              , first )

import           System.Random( Random(..)
                              , getStdRandom
                              , split )


showListOrSingleton :: Show a => [a] -> Text
showListOrSingleton [e] = pack $ show e
showListOrSingleton l   = pack $ show l

replicateElements :: Int -> [a] -> [a]
replicateElements n = concatMap (replicate n)

randomRsIO :: Random a => (a,a) -> IO [a]
randomRsIO range = getStdRandom $ split >>> first (randomRs range)
