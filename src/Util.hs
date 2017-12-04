{-# LANGUAGE NoImplicitPrelude #-}

module Util
    ( getDayIndexInMonth
    , getSeconds
    , showListOrSingleton
    , replicateElements
    , takeWhileInclusive
    , randomRsIO
    , range
    , commonPrefix
    , commonSuffix
    ) where

import           Imajuscule.Prelude

import           Data.List(reverse)
import           Data.String(String)
import           Data.Text(Text, pack)
import           Data.Time( UTCTime(..), toGregorian )

import           Control.Arrow( (>>>)
                              , first )

import           System.Random( Random(..)
                              , getStdRandom
                              , split )


{-# INLINABLE showListOrSingleton #-}
showListOrSingleton :: Show a => [a] -> Text
showListOrSingleton [e] = pack $ show e
showListOrSingleton l   = pack $ show l

{-# INLINE replicateElements #-}
replicateElements :: Int -> [a] -> [a]
replicateElements n = concatMap (replicate n)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x
                                    then
                                      takeWhileInclusive p xs
                                    else
                                      []

{-# INLINABLE range #-}
range :: Enum a => Ord a
      => a
      -> a
      -> [a]
range n m = if m < n then [n,(pred n)..m] else [n..m]

randomRsIO :: Random a => (a,a) -> IO [a]
randomRsIO range_ = getStdRandom $ split >>> first (randomRs range_)

getDayIndexInMonth :: UTCTime -> Int
getDayIndexInMonth (UTCTime day _) =
  let (_, _, dayOfMonth) = toGregorian day
  in pred dayOfMonth -- index start at 0

getSeconds :: UTCTime -> Int
getSeconds (UTCTime _ t) = floor t

commonPrefix :: String -> String -> String
commonPrefix (x:xs) (y:ys)
    | x == y    = x : commonPrefix xs ys
commonPrefix _ _ = []

commonSuffix :: String -> String -> String
commonSuffix s s' = reverse $ commonPrefix (reverse s) (reverse s')
