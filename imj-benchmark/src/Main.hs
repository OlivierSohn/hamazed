

module Main where

import Criterion.Main

import Imj.Benchmark.ListEltAccess

main :: IO()
main = do
  let l = [0..1000 :: Int]
      usingSubscrOp = subscrOp l
      usingHeadDrop = headDrop l
      listEltAccess n =
          [ bench ("SubscrOp " ++ show n) $ nf usingSubscrOp n
          , bench ("HeadDrop " ++ show n) $ nf usingHeadDrop n
          ]

  defaultMain [
    bgroup "listEltAccess" $
      concatMap listEltAccess [1,2,3,4,5,10,100,1000]
    ]
