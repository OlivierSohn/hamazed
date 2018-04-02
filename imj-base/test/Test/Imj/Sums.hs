{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Sums
         ( testSums
         ) where

import           Imj.Prelude
import           Prelude(logBase)
import           Control.Exception (evaluate)
import           Data.List(foldl', length, replicate)
import qualified Data.Set as Set(fromList, toList, empty, singleton, filter, size)
import           Data.Text(pack)
import           System.IO(putStr, putStrLn)

import qualified Imj.Data.Tree as Filt(Filterable(..))
import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString hiding(putStrLn, putStr)
import qualified Imj.Graphics.Text.ColorString  as CS(putStr)
import           Imj.Sums
import           Imj.Timing

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
  verify $ mkSumsStrict . Set.fromList
  verify $ mkSumsStrict2 . Set.fromList
  verify $ mkSumsArray' . Set.fromList
  verify $ mkSumsArray'' . Set.fromList
  verify $ mkSumsLazy . Set.fromList

  -- Using different implementations to find the number
  -- of different combinations of length < 6.
  -- The fastest way is to use a 'StrictTree' ('mkSumsStrict').

  let !numbers = Set.fromList maxHamazedNumbers
      measure n countCombinations =
        time $ void $ evaluate $
          countCombinations numbers (quot (maxSum * n) n) -- trick to force a new evaluation
      tests =
        [ (\a b -> Set.size         $ Set.filter   (\s -> Set.size s < 6) $ mkSums        a b            , "mkSums filter")
        , (\a b -> Set.size         $ Set.filter   (\s -> Set.size s < 6) $ mkSumsArray   a b            , "mkSumsArray filter")
        , (\a b -> length           $ filter       (\s -> length   s < 6) $ mkSumsArray'  a b            , "mkSumsArray' filter")
        , (\a b -> Set.size         $ Set.filter   (\s -> length   s < 6) $ mkSumsArray'' a b            , "mkSumsArray'' filter")
        , (\a b -> Filt.countValues $ Filt.filter  (\s -> length   s < 6) $ mkSumsStrict  a b            , "mkSumsStrict filter")
        , (\a b -> Filt.countValues $ Filt.filter  (\s -> length   s < 6) $ mkSumsLazy    a b            , "mkSumsLazy filter")
        , (\a b -> Set.size                                             $ mkSums       a b               , "mkSums")
        , (\a b -> Set.size                                             $ mkSumsArray  a b               , "mkSumsArray")
        , (\a b -> Filt.countValues                                     $ mkSumsArray' a b               , "mkSumsArray'")
        , (\a b -> Set.size                                             $ mkSumsArray'' a b               , "mkSumsArray''")
        , (\a b -> Filt.countValues                                     $ mkSumsStrict2 a b              , "mkSumsStrict2")
        , (\a b -> Filt.countValues                                     $ mkSumsStrict a b               , "mkSumsStrict")
        , (\a b -> Filt.countValues                                     $ mkSumsLazy   a b               , "mkSumsLazy")
        , (\a b -> Filt.countValues $ Filt.filter  (\s -> length s < 6) $ mkSumsStrictN  (Set.toList a) b, "mkSumsStrictN filter")
        , (\a b -> length           $ Filt.filter  (\s -> length s < 6) $ mkSumsN        (Set.toList a) b, "mkSumsN filter'")
        , (\a b -> Filt.countValues                                     $ mkSumsStrictN  (Set.toList a) b, "mkSumsStrictN")
        , (\a b -> Filt.countValues                                     $ mkSumsStrictN2 (Set.toList a) b, "mkSumsStrictN2")
        , (\a b -> Filt.countValues                                     $ mkSumsN        (Set.toList a) b, "mkSumsN")
        ]
  let nTestRepeat = 100
  times <-
    mapM
      (\n -> mapM (measure n . fst) tests)
      [1..nTestRepeat] :: IO [[Time Duration System]]
  printTimes $
    zip
      (map snd tests) $
      map
        (round . (/ (fromIntegral nTestRepeat :: Float)) . fromIntegral . toMicros) $
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

printTimes :: [(String, Int64)] -> IO ()
printTimes times = do
  putStrLn "micros|Logarithmic scale"
  mapM_ (\(desc, dt) -> do
    let n = round $ countStars dt
        s = show dt
        s' = replicate (nCharsTime - length s) ' ' ++ s
        inColor =
          colored (pack $ replicate n '+') green <>
          colored (pack $ replicate (nStars - n) '.') (gray 14)
    putStr $ s' ++ " "
    CS.putStr inColor
    putStr $ " " ++ desc ++ "\n") times
 where
  nCharsTime = length $ show $ maximum $ map snd times
  nStars = 120
  countStars dt =
    let d = logBase minD (fromIntegral dt)
    in succ $ fromIntegral (pred nStars) * (d - 1) / (ratioMax - 1)
  ratioMax = logBase minD maxD
  maxD = fromIntegral $ maximum $ map snd times
  minD = fromIntegral $ minimum $ map snd times :: Float
time :: IO () -> IO (Time Duration System)
time action = do
  start <- getSystemTime
  action
  end <- getSystemTime
  return $ start...end

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
