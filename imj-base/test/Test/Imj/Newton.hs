{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Newton
         ( testNewton
         ) where

import           Imj.Prelude
import           Imj.Data.AlmostFloat

import           Imj.Math.Root.Newton

testNewton :: IO ()
testNewton = do
  almostRoot (const 1)     (const 0)    (-1)   1 `shouldBe` Nothing
  almostRoot id            (const 1)    1      2 `shouldBe` Nothing

  almostRoot id            (const 1)    (-1)   0 `shouldBe` Just (0, Exact)
  almostRoot id            (const 1)    (-1)   1 `shouldBe` Just (0, Exact)
  almostRoot id            (const 1)    0      1 `shouldBe` Just (0, Exact)
  almostRoot id            (const 1)    0      0 `shouldBe` Just (0, Exact)
  almostRoot id            (const 1)    (-100) 1 `shouldBe` Just (0, Exact)
  almostRoot (negate . id) (const $ -1) (-1)   0 `shouldBe` Just (0, Exact)
  almostRoot (negate . id) (const $ -1) (-1)   1 `shouldBe` Just (0, Exact)
  almostRoot (negate . id) (const $ -1) 0      1 `shouldBe` Just (0, Exact)
  almostRoot (negate . id) (const $ -1) 0      0 `shouldBe` Just (0, Exact)
  almostRoot (negate . id) (const $ -1) (-100) 1 `shouldBe` Just (0, Exact)

  almostRoot (\x -> x^(2::Int) + x) (\x -> 2*x + 1)      (-10) (-0.1) `shouldBe` Just (-1, Exact)
  almostRoot (\x -> x^(2::Int) - 1) (\x -> 2*x)          0     10     `shouldBe` Just (1, Exact)
  almostRoot (\x -> x^(2::Int) - 1) (\x -> 2*x)          (-10) 0      `shouldBe` Just (-1, Exact)
  almostRoot (\x -> x^(3::Int))     (\x -> 3*x^(2::Int)) (-10) (10)   `shouldBe` Just (0, Exact)
  almostRoot (\x -> x^(2::Int))     (\x -> 2*x)          (-10) (10)   `shouldBe` Just (0, Exact)
  almostRoot (\x -> x^(2::Int))     (\x -> 2*x)          (-1) (10) `shouldBe` Just (0, Exact)
  almostRoot (\x -> x^(3::Int))     (\x -> 3*x^(2::Int)) (-1) (10) `shouldBe` Just (0, Exact)

 where

  almostRoot a b c d =
    fmap
      (\(r,p) -> (almost $ realToFrac r,p))
      $ findRoot a b c d

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
