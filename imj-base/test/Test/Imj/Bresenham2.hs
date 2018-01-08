{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE BangPatterns #-}

module Test.Imj.Bresenham2(testBres2) where

import           Imj.Geo.Discrete.Bresenham

testBres2 :: IO Bool
testBres2 = do
  let n = 8 :: Int
      pairs = [((x,y),(x',y')) | x  <- [0..n], y  <- [0..n],
                                 x' <- [0..n], y' <- [0..n]]
  l <- mapM test pairs
  let s = sum l
  return $ length pairs == s -- True on success (on error, an error has already terminated the program)


-- | returns 1 on success, else errors
test :: ((Int, Int),(Int, Int)) -> IO Int
test (from, to) = do
  putStrLn $ show from ++ show to
  let d = blaLength from to
      br = take d $ bla from to
      !res -- we use a bang here so that it is concomittent with previous putStrLn
        | length br /= d  = error "different lengths"
        | head br /= from = error "wrong head"
        | last br /= to   = error $ show from ++ show to ++ "wrong last " ++ show (last br)
        | verifyDistances br = error $ show from ++ show to ++ "wrong distances" ++ show br
        -- now the bresenham line is valid
        | otherwise =  1
  return res

verifyDistances :: [(Int,Int)] -> Bool
verifyDistances []  = False
verifyDistances [_] = False
verifyDistances l@((x,y):(x',y'):_) =
  let dist = max (abs (x-x')) $ abs (y-y')
  in  dist /= 1 || verifyDistances (tail l)
