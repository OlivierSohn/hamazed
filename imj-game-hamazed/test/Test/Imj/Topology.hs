{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.Topology
          ( testTopology
          ) where

import           Imj.Prelude
import           Prelude(print, putStrLn)

import           Imj.Data.Matrix.Cyclic

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space.Read
import           Imj.Game.Hamazed.World.Space
import           Imj.Util

testTopology :: IO ()
testTopology = do
  testAirOnEveryFronteer
  testNumComps
  testComponentsSizesWellDistributed
  testComponentsNearby

forAnyNumberOfComponents :: (ComponentCount -> IO ())
                         -> IO ()
forAnyNumberOfComponents = forM_ [0..10]

testAirOnEveryFronteer :: IO ()
testAirOnEveryFronteer = forAnyNumberOfComponents $ \n -> do
  print n
  putStrLn "WithoutAir"
  debugForM_ matricesWithoutAirOnEveryFronteer
    (either (`shouldBe` Nothing) (error "expected Left") . matchTopology n)
  putStrLn "WithtAir"
  debugForM_ matricesWithAirOnEveryFronteer
    (either (`shouldNotBe` Nothing) (const $ return ()) . matchTopology n)

debugForM_ :: [Matrix Material]
           -> (Matrix Material -> IO ())
           -> IO ()
debugForM_ l act = forM_ l (\e -> do
  mapM_ putStrLn $ showInBox $ writeWorld e
  act e)

testNumComps :: IO ()
testNumComps = forAnyNumberOfComponents $ \n -> do
  print n
  putStrLn "WithNComponents"
  forM_ matricesWithNComponents (\(expected, m) -> getComponentCount (matchTopology n m) `shouldBe` Just expected)

testComponentsSizesWellDistributed :: IO ()
testComponentsSizesWellDistributed = do
  putStrLn "WithNNotWellDistributedComponents"
  forM_ matricesWithNNotWellDistributedComponents
    (\(expected, m) ->
      either (`shouldBe` Just expected) (error "expected Left") $ matchTopology expected m)
  putStrLn "WithNWellDistributedComponentsSpaceWellUsed"
  forM_ matricesWithNWellDistributedComponentsSpaceWellUsed
    (\(expected, m) -> do
      mapM_ putStrLn $ showInBox $ writeWorld m
      either (error "expected Right") (const $ return ()) $ matchTopology expected m)

testComponentsNearby :: IO ()
testComponentsNearby = do
  putStrLn "WithNWellDistributedComponentsSpaceNotWellUsed"
  forM_ matricesWithNWellDistributedComponentsSpaceNotWellUsed
    (\(expected, m) ->
      either (`shouldBe` Just expected) (error "expected Left") $ matchTopology expected m)

matricesWithoutAirOnEveryFronteer :: [Matrix Material]
matricesWithoutAirOnEveryFronteer = map readWorld [
 ["     "
 ,"     "
 ,"    O"]
 ,
 ["    O"
 ,"     "
 ,"     "]
 ,
 ["  O  "
 ,"     "
 ,"O    "]
 ,
 ["    O"
 ,"O    "
 ,"     "]
 ,
 ["     "
 ,"O   O"
 ,"  O  "]
 ,
 ["  O  "
 ,"O    "
 ,"  O  "]
 ,
 ["  O  "
 ,"O   O"
 ,"     "]
 ,
 ["  O  "
 ,"    O"
 ,"  O  "]
 ,
 ["OOOO "
 ,"OOOO "
 ,"OOOO "]
 ,
 ["     "
 ,"OOOOO"
 ,"OOOOO"]
 ,
 [" OOOO"
 ," OOOO"
 ," OOOO"]
 ,
 ["OOOOO"
 ,"OOOOO"
 ,"     "]
 ]

matricesWithAirOnEveryFronteer :: [Matrix Material]
matricesWithAirOnEveryFronteer = map readWorld [
 ["O    "
 ,"     "
 ,"    O"]
 ,
 ["    O"
 ,"     "
 ,"O    "]
 ,
 ["  O  "
 ,"    O"
 ,"O    "]
 ,
 ["    O"
 ,"O    "
 ,"  O  "]
 ,
 ["  O  "
 ,"O   O"
 ,"  O  "]
 ,
 ["OOOOO"
 ,"O   O"
 ,"OOOOO"]
 ,
 ["OOOOO"
 ,"OOOOO"
 ,"OOOOO"]
 ]


matricesWithNComponents :: [(ComponentCount,Matrix Material)]
matricesWithNComponents = map (fmap readWorld) [
 (2,
 ["O    "
 ,"     "
 ,"    O"])
 ,
 (3,
 ["  O  "
 ,"    O"
 ,"O    "])
 ,
 (4,
 ["  O  "
 ,"O   O"
 ,"  O  "])
 ,
 (8,
 ["O O O"
 ," O O "
 ,"O O O"])
 ,
 (1,
 ["OOOOO"
 ,"O   O"
 ,"OOOOO"])
 ,
 (1,
 ["OOOOO"
 ,"OOOOO"
 ,"OOOOO"])
 ,
 (1,
 ["OOOOO"
 ,"O   O"
 ,"O OOO"
 ,"O    "
 ,"OOOOO"
 ,"    O"
 ,"OOO O"
 ,"O   O"
 ,"OOOOO"])
 ,
 (1,
 ["O OOO"
 ,"O O O"
 ,"OOO O"])
 ]

matricesWithNWellDistributedComponentsSpaceNotWellUsed :: [(ComponentCount,Matrix Material)]
matricesWithNWellDistributedComponentsSpaceNotWellUsed = map (fmap readWorld) [
 (2,
 ["O    "
 ,"     "
 ,"    O"])
 ,
 (3,
 ["  O  "
 ,"    O"
 ,"O    "])
 ,
 (4,
 ["  O  "
 ,"O   O"
 ,"  O  "])
 ,
 (2,
 ["O  O "
 ,"O  OO"
 ,"O  O "])
 ,
 (2,
 ["O   O"
 ,"OO  O"
 ,"OO  O"])
 ,
 (2,
 ["OOO  "
 ,"    O"
 ,"   OO"])
 ,
 (2,
 ["   OO"
 ,"    O"
 ,"OOO  "])
 ,
 (2,
 ["  OOO"
 ,"O    "
 ,"OO   "])
 ,
 (2,
 ["OO   "
 ,"O    "
 ,"  OOO"])
 ]

matricesWithNWellDistributedComponentsSpaceWellUsed :: [(ComponentCount,Matrix Material)]
matricesWithNWellDistributedComponentsSpaceWellUsed = map (fmap readWorld) [
 (8,
 ["O O O"
 ," O O "
 ,"O O O"])
 ,
 (5,
 ["O   O"
 ," O O "
 ,"  O  "])
 ,
 (5,
 ["  O  "
 ," O O "
 ,"O   O"])
 ,
 (8,
 ["  O  "
 ," O O "
 ,"O   O"
 ," O O "
 ,"  O  "])
 ,
 (9,
 ["O   O"
 ," O O "
 ,"  O  "
 ," O O "
 ,"O   O"])
 ,
 (7,
 ["O O O"
 ,"     "
 ,"    O"
 ," O   "
 ,"  O O"])
 ,
 (7,
 ["O O  "
 ,"   O "
 ,"    O"
 ,"   O "
 ,"O O  "])
 ,
 (7,
 ["O   O"
 ,"     "
 ,"O   O"
 ," O O "
 ,"  O  "])
 ,
 (2,
 ["OOOOO"
 ,"     "
 ,"OOOOO"])
 ,
 (3,
 ["OO OO"
 ,"O O O"
 ,"O OO "])
 ,
 (3,
 ["O O O"
 ,"O O O"
 ,"O O O"])
 ,
 (2,
 ["OOO  "
 ,"   OO"
 ,"    O"])
 ,
 (2,
 ["    O"
 ,"   OO"
 ,"OOO  "])
 ,
 (2,
 ["  OOO"
 ,"OO   "
 ,"O    "])
 ,
 (2,
 ["O    "
 ,"OO   "
 ,"  OOO"])
 ,
 (5,
 ["OOOOO"
 ,"     "
 ,"O OOO"
 ,"O    "
 ,"OO O "
 ,"   OO"
 ,"  O O"
 ,"OOO O"
 ,"  O  "])
 ,
 (2,
 ["O OOO"
 ,"O O O"
 ,"OO  O"])
 ]

matricesWithNNotWellDistributedComponents :: [(ComponentCount,Matrix Material)]
matricesWithNNotWellDistributedComponents = map (fmap readWorld) [
 (2,
 ["OOO  "
 ,"     "
 ,"    O"])
 ,
 (3,
 ["  O  "
 ,"    O"
 ,"O  OO"])
 ,
 (4,
 ["O O O"
 ,"O   O"
 ,"O OO "])
 ,
 (6,
 ["O OOO"
 ," O O "
 ,"O O O"])
 ,
 (2,
 ["OO   "
 ,"     "
 ,"OOOOO"])
 ,
 (3,
 ["OOOOO"
 ,"O   O"
 ,"O OOO"
 ,"     "
 ,"O OOO"
 ,"    O"
 ,"OOO O"
 ,"O   O"
 ,"OOOOO"])
 ]



shouldNotBe :: (Show a, Eq a) => a -> a -> IO ()
shouldNotBe actual unexpected =
  when (actual == unexpected) $ error $ "didn't expect\n" ++ show unexpected


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  unless (actual == expected) $ error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
