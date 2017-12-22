{-# LANGUAGE BangPatterns #-}

module Test.Imj.Bresenham3(testBres3) where

import Imj.Geo.Discrete.Bresenham3

testBres3 :: IO Bool
testBres3 = do
  let n = 8 :: Int
      pairs = [((x,y,z),(x',y',z')) | x  <- [0..n], y  <- [0..n], z  <- [0..n],
                                      x' <- [0..n], y' <- [0..n], z' <- [0..n]]
  l <- mapM test pairs
  let s = sum l
  return $ length pairs == s -- True on success (on error, an error has already terminated the program)


-- | returns 1 on success, else errors
test :: ((Int, Int, Int),(Int, Int, Int)) -> IO Int
test (from, to) = do
  --putStrLn $ show from ++ show to
  let d = bresenham3Length from to
      br = bresenham3 from to
      !res -- we use a bang here so that it is concomittent with previous putStrLn
       |length br /= d  = error "different lengths"
       |head br /= from = error "wrong head"
       |last br /= to   = error $ show from ++ show to ++ "wrong last " ++ show (last br)
       |verifyDistances br = error $ show from ++ show to ++ "wrong distances" ++ show br
       -- now the bresenham line is valid
       |otherwise =  1
  return res

verifyDistances :: [(Int,Int,Int)] -> Bool
verifyDistances []  = False
verifyDistances [_] = False
verifyDistances l@((x,y,z):(x',y',z'):_) =
  let dist = max (abs (x-x')) (max (abs (y-y')) (abs (z-z')))
  in  dist > 1 || verifyDistances (tail l)
