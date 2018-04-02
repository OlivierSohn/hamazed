{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.CyclicMatrix
           ( testCyclicMatrix
           ) where

import           Imj.Prelude
import           Prelude(print, take)
import           Imj.Data.Matrix.Cyclic

testCyclicMatrix :: IO ()
testCyclicMatrix = do
  let m = fromList 3 5 $ take 30 [0 :: Int ..]
  mapM_ print $ produceRotations Order1 m
