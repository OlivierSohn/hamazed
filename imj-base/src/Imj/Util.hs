{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Util
    ( -- * List utilities
      showListOrSingleton
    , replicateElements
    , intersperse'
    , splitEvery
    , mkGroups
    , range
    , takeWhilePlus
    , commonPrefix
    , commonSuffix
    , maximumMaybe
      -- * Math utilities
    , clamp
    , zigzag
    , lastAbove
    , logBase2
    , mapRange
    , Quantifiable(..)
    -- * distribution utilities
    , asDistribution
    , Distribution
    -- * Render utilities
    , showArray
    , showArrayN
    , showDistribution
    , showQuantities
    , showInBox
    , addRight
    , justifyR
    , justifyL
    -- * Reexports
    , Int64
    ) where

import           Imj.Prelude

import           Data.Bits(finiteBitSize, countLeadingZeros)
import           Data.Int(Int64)
import           Data.List(reverse, length, splitAt, foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe(listToMaybe)
import           Data.Text(Text, pack)

{-# INLINE maximumMaybe #-}
maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs

addRight :: [String] -> Int -> [String] -> [String]
addRight l1' margin l2' =
  zipWith (++)
    (fillH l1 maxWidth1)
    (fillH l2 maxWidth2)
 where
  fillH x maxWidth = take height $ x ++ repeat (replicate maxWidth ' ')
  maxWidth1 = maxLength l1'
  maxWidth2 = maxLength l2'
  height = max (length l1') (length l2')
  fillW maxWidth = map (\str -> take maxWidth $ str ++ repeat ' ')
  l1 = fillW (maxWidth1 + margin) l1'
  l2 = fillW maxWidth2 l2'

maxLength :: [[a]] -> Int
maxLength = fromMaybe 0 . maximumMaybe . map length

showInBox :: [String] -> [String]
showInBox l =
  bar '_' : map (withFrame '|') l ++ [bar 'T']
 where
  bar = replicate (maxWidth + 2)
  withFrame f str = f : take maxWidth (str ++ repeat ' ') ++ [f]
  maxWidth = maxLength l

showArray :: Maybe (String, String) -> [(String,String)] -> [String]
showArray mayTitles body =
  showArrayN
    (fmap pairToList mayTitles)
    (map pairToList body)
 where pairToList (a,b) = [a,b]

showArrayN :: Maybe [String] -> [[String]] -> [String]
showArrayN mayTitles body =
  maybe [] (\titles -> bar : format [titles]) mayTitles
  ++ [bar] ++ format body ++ [bar]
 where
  bar = replicate lBar '-'
  lBar = maxLength $ format arrayLines
  format =
    map
      (\strs ->
      "| " ++
      intercalate
        " | "
        (map
          (\(columnIdx, str, justify) -> justify (ls !! columnIdx) str)
          $ zip3 [0..] strs justifications) ++
      " |")
  arrayLines = maybe body (:body) mayTitles
  ls = map
    (\colIdx -> maxLength $ mapMaybe (listToMaybe . drop colIdx) arrayLines)
    [0..]

  justifications = justifyL : repeat justifyR

justifyR, justifyL :: Int -> String -> String
justifyR maxL x =
  replicate (maxL-length x) ' ' ++ x
justifyL maxL x =
  x ++ replicate (maxL-length x) ' '


class (Ord a, Show a) => Quantifiable a where
  readFloat :: Float -> a
  writeFloat :: a -> Float

  -- | Can be seen as a zero, and is the result of 'partition 0 x'.
  nothing :: a
  nothing = readFloat 0

  gather :: a -> a -> a
  gather a b = readFloat $ writeFloat a + writeFloat b

  scatter :: Int -> a -> a
  scatter 0 _ = nothing
  scatter n a = readFloat $ writeFloat a / fromIntegral n

  average :: [a] -> a
  average l = scatter (length l) $ foldl' gather nothing l

  normalize :: [a] -> [Float]
  normalize l =
    let fs = map writeFloat l
        maxQty = fromMaybe (error "logic") $ maximumMaybe fs
    in  map (/maxQty) fs

  showQty :: a -> String
  showQty = show

instance Quantifiable Float where
  readFloat = realToFrac
  writeFloat = realToFrac

showQuantities :: (Quantifiable a)
               => [a]
               -- ^ value
               -> [String]
showQuantities l' =
  showArrayN (listToMaybe header) body
 where
  l = avg : l'
  avg = average l'
  txts = map showQty l
  normalizedQuantities = normalize l
  graphical = map (\q -> replicate (round $ q*50) '|') normalizedQuantities
  (header, body) = splitAt 1
    $ map (\(i,g,t) -> [i, g, t])
    $ zip3
        ("Avg" : map show [1 :: Int ..])
        graphical
        txts

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
      in justifyL maxWidth s ++ " | " ++ replicate n '.')
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
  maxWidth = maxLength $ map (show . fst) l

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

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs


-- | Takes elements matching a condition, and the element thereafter.
takeWhilePlus :: (a -> Bool) -> [a] -> [a]
takeWhilePlus _ [] = []
takeWhilePlus p (x:xs) =
  x : bool [] (takeWhilePlus p xs) (p x)

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

{-# INLINABLE mapRange #-}
mapRange :: (Fractional a, Eq a)
         => a
         -- ^ low 1
         -> a
         -- ^ high 1
         -> a
         -- ^ low 2
         -> a
         -- ^ high 2
         -> a
         -- ^ value 1
         -> a
         -- ^ value 2
mapRange l1 h1 l2 h2 v1
  | denom == 0 = (h2 + l2) / 2
  | otherwise = l2 + normalized * (h2 - l2)
 where
  denom = h1 - l1
  normalized = (v1 - l1) / denom

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
