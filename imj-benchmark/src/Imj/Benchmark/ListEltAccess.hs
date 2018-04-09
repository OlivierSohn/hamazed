{-# LANGUAGE BangPatterns #-}

module Imj.Benchmark.ListEltAccess
        ( subscrOp
        , headDrop) where

tooLarge :: Int -> a
tooLarge _ = errorWithoutStackTrace "!!: index too large"

negIndex :: a
negIndex = errorWithoutStackTrace "!!: negative index"

-- this is the code of base for (!!)
subscrOp :: [a] -> Int -> a
{-# INLINABLE subscrOp #-}
subscrOp xs n
  | n < 0     = negIndex
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) tooLarge xs n

headDrop :: [a] -> Int -> a
{-# INLINABLE headDrop #-}
headDrop xs n
  | n < 0     = negIndex
  | otherwise = unsafeHeadDrop n xs
  where
    unsafeHeadDrop :: Int -> [a] -> a
    unsafeHeadDrop !_ []    = errorWithoutStackTrace "too Big index"
    unsafeHeadDrop 0  (e:_) = e
    unsafeHeadDrop m  (_:l) = unsafeHeadDrop (m - 1) l
