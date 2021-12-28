
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent(threadDelay)
import           Control.Monad(void)

import           Imj.Audio
import           Imj.Music.Random(pickRandom)
import           Imj.Music.Compositions.Me
import           Imj.Music.Compositions.Tech
import           Imj.Music.Compositions.Tchaikovski
import           Imj.Music.Compositions.Vivaldi

playShortLowNote :: Instrument -> IO (Either () ())
playShortLowNote instrument = do
  _ <- play $
    StartNote Nothing (InstrumentNote Do (Octave 6) instrument) (NoteVelocity 0.01) panCentered
  play $
    StopNote Nothing (InstrumentNote Do (Octave 6) instrument)

main :: IO ()
main = void $ usingAudioOutput -- WithMinLatency 0
     $ do
  -- comment the following line out to do benchmarks: it will generate a lot of note events
  -- in a short period of time, and allows to produce the priority inversion effect
  -- when a global lock is used:
  {-
  _ <- stressTest
  threadDelay 10000
  --}

  -- play a short snare note to initialize pink node
  putStrLn "play short & low snare note to initialize pink Noise"
  _ <- playShortLowNote meSnare
  putStrLn "playing random score"
  randInstructions1 <- pickRandom countInstructions allowedInstructions1
  randInstructions2 <- pickRandom countInstructions allowedInstructions2
  -- TODO panning : add panning info in the instrument, use StereoPanned<audioElt>
  playVoicesAtTempo 440.0 simpleInstrument
    (
    ((NotePan $ -1), ((take (countLoops * countInstructions) $ cycle randInstructions1) ++ [Rest])) :
    ((NotePan $ 1), ((take (countLoops * countInstructions) $ cycle randInstructions2) ++ [Rest])) :
    [])
    >>= print
  putStrLn "playing me"
  uncurry (playScoreAtTempo 100) (meScore $ Just 10.0) >>= print
  threadDelay 10000
  putStrLn "playing tech"
  uncurry (flip playVoicesAtTempo techInstrument) tech >>= print
  threadDelay 10000
  putStrLn "playing tech2"
  uncurry (flip playVoicesAtTempo techInstrument) tech2 >>= print
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
 where
  countLoops = 16
  countInstructions = 16
  allowedNotes = map (uncurry Note . midiPitchToNoteAndOctave) [60..80]
  allowedInstructions2 = allowedNotes ++ [Rest, Extend]
  allowedInstructions1 = allowedNotes ++ [Rest]

stressTest :: IO PlayResult
stressTest = playVoicesAtTempo 10000 simpleInstrument $ allCentered $ map (take 1000 . cycle) [voices|
  sol  - .  . .  .   la - .  . si -   -  - .
  vsol - -  - .  vla -  - -  . .  vsi -  . .
  do   . do . do .   do . do . do .   do - .
  |]
