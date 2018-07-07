
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent(threadDelay)
import           Control.Monad(void)

import           Imj.Audio
import           Imj.Music.Compositions.Tech
import           Imj.Music.Compositions.Tchaikovski
import           Imj.Music.Compositions.Vivaldi


main :: IO ()
main = void $ usingAudioOutput -- WithMinLatency 0
     $ do
  --stressTest
  --threadDelay 10000
  putStrLn "playing tech"
  uncurry (flip playVoicesAtTempo techInstrument) tech >>= print
  threadDelay 10000
  putStrLn "playing vivaldi summer presto"
  uncurry (flip playVoicesAtTempo simpleInstrument) vivaldiFourSeasonsSummerPresto >>= print
  threadDelay 10000
  putStrLn "playing vivaldi spring"
  uncurry (flip playVoicesAtTempo simpleInstrument) vivaldiFourSeasonsSpring >>= print
  threadDelay 10000
  putStrLn "playing tchaikovski swan lake"
  uncurry (flip playVoicesAtTempo simpleInstrument) tchaikovskiSwanLake >>= print
  threadDelay 10000

stressTest :: IO PlayResult
stressTest = playVoicesAtTempo 10000 simpleInstrument $ map (take 1000 . cycle) [voices|
  sol  - .  . .  .   la - .  . si -   -  - .
  vsol - -  - .  vla -  - -  . .  vsi -  . .
  do   . do . do .   do . do . do .   do - .
  |]
