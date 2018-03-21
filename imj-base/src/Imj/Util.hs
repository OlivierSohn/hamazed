{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Util
    ( -- * List utilities
      showListOrSingleton
    , replicateElements
    , intersperse'
    , mkGroups
    , range
    , takeWhileInclusive
    , commonPrefix
    , commonSuffix
      -- * Math utilities
    , clamp
    , zigzag
    , lastAbove
    , logBase2
    -- * distribution utilities
    , asDistribution
    , Distribution
    -- * Render utilities
    , showArray
    , showDistribution
    , showInBox
    -- * Reexports
    , Int64
    ) where

import           Imj.Prelude

import           Data.Bits(finiteBitSize, countLeadingZeros)
import           Data.Int(Int64)
import           Data.List(reverse, length, splitAt, foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe(maybeToList)
import           Data.Text(Text, pack)

{-# INLINE maximumMaybe #-}
maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs

showInBox :: [String] -> [String]
showInBox l =
  bar '_' : map (withFrame '|') l ++ [bar 'T']
 where
  bar = replicate (maxWidth + 2)
  withFrame f str = f : str ++ [f]
  maxWidth = fromMaybe 0 $ maximumMaybe $ map length l

showArray :: Maybe (String, String) -> [(String,String)] -> [String]
showArray mayTitles body =
  maybe [] (\titles -> bar : format [titles]) mayTitles
  ++ [bar] ++ format body ++ [bar]
 where
  bar = replicate lBar '-'
  lBar = fromMaybe 0 $ maximumMaybe $ map length $ format allArray
  format = map (\(a,b) -> "|" ++ justifyL a l1 ++ "|" ++ justifyR b l2 ++ "|")
  allArray = maybeToList mayTitles ++ body
  l1 = fromMaybe 0 $ maximumMaybe $ map (length . fst) allArray
  l2 = fromMaybe 0 $ maximumMaybe $ map (length . snd) allArray
  justifyR x maxL =
    let l = length x
    in " " ++ replicate (maxL-l) ' ' ++ x ++ " "
  justifyL x maxL =
    let l = length x
    in " " ++ x ++ replicate (maxL-l) ' ' ++ " "

type Distribution a = Map a Int

{-# INLINABLE asDistribution #-}
asDistribution :: (Ord a)
               => [a] -> Distribution a
asDistribution = Map.fromListWith (+) . map (flip (,) 1)

{-# INLINABLE showDistribution #-}
showDistribution :: (Ord a, Show a, Enum a)
                 => Distribution a
                 -> [String]
showDistribution m =
  map
    (\(k,n) ->
      let s = show k
      in s ++ replicate (maxWidth - length s) ' ' ++ " | " ++ replicate n '.')
    l
 where
  mayMin = Map.lookupMin m
  mayMax = Map.lookupMax m
  -- m' has no gap between min and max key (we add 0s if needed)
  m' = maybe m (\(min_,_) ->
      let (max_,_) = fromMaybe (error "logic") mayMax
      in foldl' (\ma k -> Map.insertWith (+) k 0 ma) m [min_..max_])
    mayMin
  l = Map.toAscList m'
  maxWidth = fromMaybe 0 $ maximumMaybe $ map (length . show . fst) l

{-# INLINABLE showListOrSingleton #-}
-- | If list is a singleton, show the element, else show the list.
showListOrSingleton :: Show a => [a] -> Text
showListOrSingleton [e] = pack $ show e
showListOrSingleton l   = pack $ show l

{-# INLINE replicateElements #-}
-- | Replicates each list element n times and concatenates the result.
replicateElements :: Int -> [a] -> [a]
replicateElements n = concatMap (replicate n)

-- | Divides a list in n lists of sizes s or s+1. Bigger lists are placed at the
-- beginning.
--
-- Elements order is maintained, i.e for every n>0 and input :
--
-- @ input == concat $ mkGroups n input @
{-# INLINABLE mkGroups #-}
mkGroups :: Int
         -- ^ number of groups, must be > 0
         -> [a]
         -> [[a]]
mkGroups n elts
  | n <= 0 = error $ "negative group count " ++ show n
  | otherwise = reverse $ assert (null remainingElts) groups
  where
    l = length elts
    (minGroupSize,remain) = quotRem l n
    sizes = replicate remain (succ minGroupSize) ++ replicate (n-remain) minGroupSize
    (remainingElts, groups) =
      foldl'
        (\(rElts,res) sz ->
          let (a,rest) = splitAt sz rElts
          in (rest,a:res))
        (elts, [])
        sizes

-- | Takes elements, until (inclusively) a condition is met.
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) =
  x : if p x
        then
          takeWhileInclusive p xs
        else
          []

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

-- | Produces an infinite triangle signal given a linear input.
{-# INLINABLE zigzag #-}
zigzag :: Integral a
       => a
       -- ^ Inclusive min
       -> a
       -- ^ Inclusive max
       -> a
       -- ^ Value
       -> a
zigzag from' to' v =
  let from = min from' to'
      to = max from' to'
      d = to-from
      v' = v `mod` (2*d)
  in from + if v' <= d
              then v'
              else
                2*d - v'

{-# INLINABLE commonPrefix #-}
commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix (x:xs) (y:ys)
    | x == y    = x : commonPrefix xs ys
commonPrefix _ _ = []

{-# INLINABLE commonSuffix #-}
commonSuffix :: (Eq a) => [a] -> [a] -> [a]
commonSuffix s s' = reverse $ commonPrefix (reverse s) (reverse s')


-- from https://hackage.haskell.org/package/text-1.2.3.0
intersperse' :: a -> [a] -> [a]
intersperse' _   []     = []
intersperse' sep (x:xs) = x : go xs
  where
    go []     = []
    go (y:ys) = sep : y: go ys
{-# INLINE intersperse' #-}

-- | Expects the bounds to be in the right order.
{-# INLINABLE clamp #-}
clamp :: Ord a
      => a
      -- ^ The value
      -> a
      -- ^ The inclusive minimum bound
      -> a
      -- ^ The inclusive maximum bound
      -> a
clamp !n min_ max_
  | n < min_ = min_
  | n > max_ = max_
  | otherwise = n


{- | Given :

* a discrete input interval @[i,j]@
* a /decreasing/ monadic function @f :: (Ord y) => Int -> m y@
* an output /threshold/ value @y@

finds @x@ in @[i,j]@ such that :

* @f(x) > y@, and
* either @y >= f(x+1)@ or @x == j@

With a time complexity of O(log(j-i)).
-}
lastAbove :: (Monad m
            , Integral x
            , Ord y)
          => y
          -- ^ Output value
          -> (x -> m y)
          -- ^ Decreasing function
          -> x
          -- ^ Inclusive min bound
          -> x
          -- ^ Inclusive max bound
          -> m (Maybe x)
lastAbove threshold f minIdx maxIdx =
  go (pred minIdx) (succ maxIdx) Nothing -- TODO refactor to first test extremities?
 where
  go tooLow tooHigh res
    -- stop when the admissible range is empty:
    | tooLow >= pred tooHigh = return res
    | otherwise = f x >>= \middleValue ->
        if middleValue > threshold
          then
            -- adjust the "best" index found, adjust the lower bound to that index and continue the search.
            go x tooHigh $ Just $ maybe x (max x) res
          else
            -- adjust the upper bound to that index (we assume that
            -- if 'condition i' is not satisfied, then for every j > i, condition j is not satisfied)
            -- and continue the search.
            go tooLow x res
      where
        x = quot (tooLow + tooHigh) 2

{-# INLINE logBase2 #-}
logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
