{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Imj.ShouldTest
          ( testShouldTest
          ) where

import           Imj.Prelude

import qualified Data.Map.Strict as Map

import           Imj.Profile.Result
import           Imj.Profile.Results
import           Imj.Space.Types
import           Imj.Space.Strategies(StrategyTag(..))

testShouldTest :: IO ()
testShouldTest = do
  let res :: MaybeResults ()
      res = MaybeResults $ Map.fromList
        [(SWCharacteristics (Size 4 10) 1 0.5, Just $ TaggedResult Refined Nothing $ TD Map.empty) -- so that (5,10) should be tested
        ,(SWCharacteristics (Size 6 10) 1 0.5, Nothing) -- so that (12,10) shouldn't be tested.
        ]

  shouldTest (SWCharacteristics (Size 5 10) 1 0.5) res `shouldBe` (ClosestSmallerSizeHas $ TD Map.empty)
  shouldTest (SWCharacteristics (Size 10 5) 1 0.5) res `shouldBe` (ClosestSmallerSizeHas $ TD Map.empty)

  shouldTest (SWCharacteristics (Size 2 10) 1 0.5) res `shouldBe` FirstSize
  shouldTest (SWCharacteristics (Size 10 2) 1 0.5) res `shouldBe` FirstSize

  shouldTest (SWCharacteristics (Size 12 10) 1 0.5) res `shouldBe` ClosestSmallerSizeHasNoResult
  shouldTest (SWCharacteristics (Size 10 12) 1 0.5) res `shouldBe` ClosestSmallerSizeHasNoResult


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
