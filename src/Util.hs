{-# LANGUAGE NoImplicitPrelude #-}

module Util
    ( showListOrSingleton
    , replicateElements
    , takeWhileInclusive
    , randomRsIO
    , range
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


takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x
                                    then
                                      takeWhileInclusive p xs
                                    else
                                      []


range :: Enum a => Ord a
      => a
      -> a
      -> [a]
range n m = if m < n then [n,(pred n)..m] else [n..m]

randomRsIO :: Random a => (a,a) -> IO [a]
randomRsIO range_ = getStdRandom $ split >>> first (randomRs range_)
