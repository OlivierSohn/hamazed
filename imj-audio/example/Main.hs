
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Concurrent(threadDelay)

import           Imj.Audio
import           Imj.Music.Compositions.Tchaikowski
import           Imj.Music.Compositions.Vivaldi

main :: IO ()
main = usingAudio $ do
  uncurry (flip playVoicesAtTempo simpleInstrument) vivaldiFourSeasonsSummerPresto
  uncurry (flip playVoicesAtTempo simpleInstrument) tchaikowskiSwanLake
  uncurry (flip playVoicesAtTempo simpleInstrument) vivaldiFourSeasonsSpring
  threadDelay 10000
