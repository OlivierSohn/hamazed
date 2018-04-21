{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Imj.Sums
         ( testSums
         ) where

import           Imj.Prelude

import           Control.Exception (evaluate)
import           Data.List(foldl', length, replicate)
import qualified Data.Set as Set
import qualified Data.IntSet as ISet
import           Data.Text(pack)
import           System.IO(putStr, putStrLn)

import           Imj.Data.Class.Quantifiable
import qualified Imj.Data.Tree as Filt(Filterable(..))
import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString hiding(putStrLn, putStr)
import qualified Imj.Graphics.Text.ColorString  as CS(putStr)
import           Imj.Sums
import           Imj.Timing
import           Imj.Util

testSums :: IO ()
testSums = do
  testAsOccurences

  mkSums Set.empty 0 `shouldBe` Set.singleton Set.empty
  mkSums (Set.fromList [1,2,3,4,5]) 3
   `shouldBe` Set.fromList (map Set.fromList [[3],[2,1]])
  mkSums (Set.fromList [2,3,4,5]) 18
   `shouldBe` Set.empty
  mkSums (Set.fromList [2,3,4,5]) 1
   `shouldBe` Set.empty
  let maxHamazedNumbers = [1..15]
      maxSum = sum maxHamazedNumbers `quot` 2

  mkSumsArray Set.empty 0 `shouldBe` Set.singleton Set.empty
  mkSumsArray (Set.fromList [1,2,3,4,5]) 3
   `shouldBe` Set.fromList (map Set.fromList [[3],[2,1]])
  mkSumsArray (Set.fromList [2,3,4,5]) 18
   `shouldBe` Set.empty
  mkSumsArray (Set.fromList [2,3,4,5]) 1
   `shouldBe` Set.empty

  -- verify all output lists are descending
  let verify x = Set.fromList (Filt.toList $ x [1,2,3,3,4,5,6,9,9] 3) `shouldBe` Set.fromList [[3],[2,1]]
  verify mkSumsN
  verify mkSumsStrictN
  verify mkSumsStrictN2
  verify $ mkSumsStrict . ISet.fromList
  verify $ mkSumsStrict2 . Set.fromList
  verify $ mkSumsArray' . Set.fromList
  verify $ mkSumsArray'' . Set.fromList
  verify $ mkSumsLazy . Set.fromList

  -- Using different implementations to find the number
  -- of different combinations of length < 6.
  -- The fastest way is to use a 'StrictTree' ('mkSumsStrict').

  let !numbersL = maxHamazedNumbers
      !numbersS = Set.fromList maxHamazedNumbers
      !numbersIS = ISet.fromList maxHamazedNumbers

      measure n countCombinations =
        fst <$> withDuration (void $ evaluate $ force $
          countCombinations numbersL numbersS numbersIS (quot (maxSum * n) n)) -- trick to force a new evaluation
      tests =
        [ (\_ aS _ b -> Set.size         $ Set.filter   (\s -> Set.size s < 6) $ mkSums        aS b            , "mkSums filter")
        , (\_ aS _ b -> Set.size         $ Set.filter   (\s -> Set.size s < 6) $ mkSumsArray   aS b            , "mkSumsArray filter")
        , (\_ aS _ b -> length           $ filter       (\s -> length   s < 6) $ mkSumsArray'  aS b            , "mkSumsArray' filter")
        , (\_ aS _ b -> Set.size         $ Set.filter   (\s -> length   s < 6) $ mkSumsArray'' aS b            , "mkSumsArray'' filter")
        , (\_ _ aIS b -> Filt.countValues $ Filt.filter  (\s -> length   s < 6) $ mkSumsStrict  aIS b , "mkSumsStrict filter")
        , (\_ aS _ b -> Filt.countValues $ Filt.filter  (\s -> length   s < 6) $ mkSumsLazy    aS b            , "mkSumsLazy filter")
        , (\_ aS _ b -> Set.size                                             $ mkSums          aS b               , "mkSums")
        , (\_ aS _ b -> Set.size                                             $ mkSumsArray     aS b               , "mkSumsArray")
        , (\_ aS _ b -> Filt.countValues                                     $ mkSumsArray'    aS b               , "mkSumsArray'")
        , (\_ aS _ b -> Set.size                                             $ mkSumsArray''   aS b               , "mkSumsArray''")
        , (\_ aS _ b -> Filt.countValues                                     $ mkSumsStrict2   aS b              , "mkSumsStrict2")
        , (\_ _ aIS b -> Filt.countValues                                     $ mkSumsStrict    aIS b , "mkSumsStrict")
        , (\_ aS _ b -> Filt.countValues                                     $ mkSumsLazy      aS b               , "mkSumsLazy")
        , (\aL _ _ b -> Filt.countValues $ Filt.filter  (\s -> length s < 6) $ mkSumsStrictN   aL b, "mkSumsStrictN filter")
        , (\aL _ _ b -> length           $ Filt.filter  (\s -> length s < 6) $ mkSumsN         aL b, "mkSumsN filter'")
        , (\aL _ _ b -> Filt.countValues                                     $ mkSumsStrictN   aL b, "mkSumsStrictN")
        , (\aL _ _ b -> Filt.countValues                                     $ mkSumsStrictN2  aL b, "mkSumsStrictN2")
        , (\aL _ _ b -> Filt.countValues                                     $ mkSumsN         aL b, "mkSumsN")
        ]
  let nTestRepeat = 100
  times <- (numbersL, numbersS, numbersIS) `deepseq`
    mapM
      (\n -> mapM (measure n . fst) tests)
      [1..nTestRepeat] :: IO [[Time Duration System]]
  printTimes $
    zip
      (map snd tests) $
      map
        (readFloat . (/ (fromIntegral nTestRepeat :: Float)) . writeFloat) $
        foldl'
          (zipWith (|+|))
          (repeat zeroDuration)
          times

testAsOccurences :: IO ()
testAsOccurences = do
  asOccurences [] `shouldBe` []
  asOccurences [3] `shouldBe` [ValueOccurences 1 3]
  asOccurences [3,3] `shouldBe` [ValueOccurences 2 3]
  asOccurences [3,4] `shouldBe` [ValueOccurences 1 4, ValueOccurences 1 3] -- Note : reversed
  asOccurences [3,3,5,5,5,6,7,7,9,10]
   `shouldBe`
    [ ValueOccurences 1 10
    , ValueOccurences 1 9
    , ValueOccurences 2 7
    , ValueOccurences 1 6
    , ValueOccurences 3 5
    , ValueOccurences 2 3
    ]

printTimes :: [(String, Time Duration System)] -> IO ()
printTimes [] = putStrLn "No time"
printTimes times = do
  putStrLn "micros|Logarithmic scale"
  forM_ (zip times logTimes) $ \((desc, dt), logRatio) -> do
    let n = countStars logRatio
        s = showTime dt
        s' = replicate (nCharsTime - length s) ' ' ++ s
        inColor =
          colored (pack $ replicate n '+') (gray 19) <>
          colored (pack $ replicate (nStars - n) '.') (gray 5)
    putStr $ s' ++ " "
    CS.putStr inColor
    putStr $ " " ++ desc ++ "\n"
 where
  nCharsTime = length $ showTime $ fromMaybe (error "logic") $ maximumMaybe $ map snd times
  nStars = 120
  countStars x = round $ fromIntegral nStars * x
  logTimes = logarithmically 10 $ map snd times


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
