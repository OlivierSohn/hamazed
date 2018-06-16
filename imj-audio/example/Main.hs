
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent(threadDelay)

import           Imj.Audio
import           Imj.Music.Compositions.Tchaikowski
import           Imj.Music.Compositions.Vivaldi


-- to be lock free, we would need lock free queues for:
-- computes, orchestrators
-- https://github.com/cameron314/readerwriterqueue/blob/master/readerwriterqueue.h

main :: IO ()
main = usingAudio -- WithMinLatency 0.0021
    $ do
  -- this example produces, in debug and with Soundflower which has a 1.4ms default latency :
  --    with no latency : overflow flag: 0 0 4 0 0 (at the end of the first loop)
  --      - which means we take too long to compute (to fix it we should be lock-free?)
  --      - the overflow goes away with 0.002s minLatency
  --    with latency 0.04 : visible audio glitches  (see sample 916552)
  playVoicesAtTempo 10000 simpleInstrument $ map (take 1000 . cycle) [voices|
    sol  - .  . .  .   la - .  . si -   -  - .
    vsol - -  - .  vla -  - -  . .  vsi -  . .
    do   . do . do .   do . do . do .   do - .

  : 3      1    1  1   2    1    2  1   1        <- note on
  :      1 1  1 1  1      1 1  2    1      1 2   <- note off
  : 3    1 2  1 2  2   2  1 2  2 2  2   1  1 2
    |]
  putStrLn "vivaldi"
  uncurry (flip playVoicesAtTempo simpleInstrument) vivaldiFourSeasonsSummerPresto
  uncurry (flip playVoicesAtTempo simpleInstrument) tchaikowskiSwanLake
  uncurry (flip playVoicesAtTempo simpleInstrument) vivaldiFourSeasonsSpring
  threadDelay 10000
