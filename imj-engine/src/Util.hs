{-# LANGUAGE NoImplicitPrelude #-}

module Util
    ( -- * Time
      getDayIndexInMonth
    , getSeconds
      -- * List
    , showListOrSingleton
    , replicateElements
    , range
      -- * String
    , commonPrefix
    , commonSuffix
      -- * Random numbers
    , randomRsIO
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
-- | If list is a singleton, show the element, else show the list.
showListOrSingleton :: Show a => [a] -> Text
showListOrSingleton [e] = pack $ show e
showListOrSingleton l   = pack $ show l

{-# INLINE replicateElements #-}
-- | Replicates each list element n times and concatenates the result.
replicateElements :: Int -> [a] -> [a]
replicateElements n = concatMap (replicate n)

{-# INLINABLE range #-}
{- | Builds a range with no constraint on the order of bounds:

@
range 3 5 == [3,4,5]
range 5 3 == [5,4,3]
@
-}
range :: Enum a => Ord a
      => a -- ^ First inclusive bound
      -> a -- ^ Second inclusive bound
      -> [a]
range n m =
  if m < n
    then
      [n,(pred n)..m]
    else
      [n..m]

-- | Returns a list of random values uniformly distributed in the closed interval
-- [lo,hi].
--
-- It is unspecified what happens if lo>hi
randomRsIO :: Random a
           => a -- ^ lo : lower bound
           -> a -- ^ hi : upper bound
           -> IO [a]
randomRsIO from to =
  getStdRandom $ split >>> first (randomRs (from, to))

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
