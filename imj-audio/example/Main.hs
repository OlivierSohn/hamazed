
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent(threadDelay)

import           Imj.Audio
import           Imj.Music.Compositions.Tchaikowski
import           Imj.Music.Compositions.Vivaldi


main :: IO ()
main = usingAudio $ do
  uncurry (flip playVoicesAtTempo simpleInstrument) tchaikowskiSwanLake
  threadDelay 10000
  uncurry (flip playVoicesAtTempo simpleInstrument) vivaldiFourSeasonsSpring
  threadDelay 10000
  uncurry (flip playVoicesAtTempo simpleInstrument) vivaldiFourSeasonsSummerPresto
  threadDelay 10000
  stressTest
  threadDelay 10000

stressTest :: IO ()
stressTest = playVoicesAtTempo 10000 simpleInstrument $ map (take 1000 . cycle) [voices|
  sol  - .  . .  .   la - .  . si -   -  - .
  vsol - -  - .  vla -  - -  . .  vsi -  . .
  do   . do . do .   do . do . do .   do - .
  |]
