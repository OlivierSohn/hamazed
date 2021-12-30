
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent(threadDelay)
import           Control.Monad(void)

import           System.Random.MWC(create, GenIO)
import           Data.Bool(bool)
import           Data.List(intersperse)

import           Imj.Audio hiding (intersperse)
import           Imj.Music.Random(pickRandomWeighted, pickRandomInstructionsWeighted)
import           Imj.Music.Harmony
import           Imj.Music.Compositions.Me
import           Imj.Music.Compositions.Tech
import           Imj.Music.Compositions.Tchaikovski
import           Imj.Music.Compositions.Vivaldi

playShortLowNote :: Instrument -> VoiceId -> IO (Either () ())
playShortLowNote instrument v = do
  _ <- play
    (StartNote Nothing (InstrumentNote Do (Octave 6) instrument) (NoteVelocity 0.01) panCentered)
    v
  play
    (StopNote Nothing (InstrumentNote Do (Octave 6) instrument))
    v


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
  -- play a short snare note to initialize pink noise
  putStrLn "play short & low snare note to initialize pink Noise"
  _ <- playShortLowNote meSnare $ VoiceId 0
  threadDelay 10000
  putStrLn "playing neighbour patterns"
  playNeighbourPatterns >>= print
  threadDelay 10000
  putStrLn "playing random score"
  playRandomScore >>= print
  threadDelay 10000
  putStrLn "playing me"
  uncurry (playScoreAtTempo 1) (meScore $ Just 10.0) >>= print
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


-- sumWeightsPattern / length allNotes is the probability of a note in the pattern
-- sumWeightsNotPattern / length allNotes is the probability of a note outside the pattern
weightedNotesUsingPattern :: NotesPattern AnyOffset
                          -> [MidiPitch]
                          -> Float
                          -> Float
                          -> Float
                          -> Float
                          -> [(Instruction, Float)]
weightedNotesUsingPattern pattern allNotes sumWeightsPattern sumWeightsNotPattern weightExtend weightRest =
  let l = fromIntegral $ length allNotes :: Float
      weightNotPattern = sumWeightsNotPattern / l :: Float
      weightPattern = sumWeightsPattern / l :: Float
      weightedNotes =  map
        (\pitch -> (
          uncurry Note $ midiPitchToNoteAndOctave pitch,
          bool weightNotPattern weightPattern
            $ inPattern pattern pitch))
        allNotes
  in weightedNotes ++ [(Rest, weightRest), (Extend, weightExtend)]

buildVoices :: NotesPattern AnyOffset
            -> NotesPattern AnyOffset
            -> [MidiPitch]
            -> ([(Instruction, Float)], [(Instruction, Float)])
buildVoices leftPattern rightPattern rangeNotes = (,)
  (weightedNotesUsingPattern leftPattern rangeNotes
    10 0 3 1)
  (weightedNotesUsingPattern rightPattern rangeNotes
    10 0 3 1)

playLoop :: GenIO -> Int -> [Instruction] -> [Instruction] -> IO PlayResult
playLoop rng countSteps leftInsns rightInsns = do
  pedal <- pickRandomWeighted rng countSteps [(True, 0.1), (False, 0.3)]
  playVoicesAtTempoPedal 440.0 simpleInstrument
    (
    ((NotePan $ -1), ((take countSteps $ cycle leftInsns))) :
    ((NotePan $ 1), ((take countSteps $ cycle rightInsns))) :
    [])
    pedal

playRandomScore :: IO PlayResult
playRandomScore = do
  rng <- create
  playModes (concat $ take 2 $ repeat patterns) rng
 where
  patterns = map (\(n, note, mode) -> (n, mkDefaultPattern Chord note mode, mkDefaultPattern Chord note mode)) modes
  modes =
    [ (1, Do, Major)
    , (1, Mi, Major)
    , (1, La, Minor)
    , (1, Sol, Major)]

playNeighbourPatterns :: IO PlayResult
playNeighbourPatterns = do
  rng <- create
  playModes (map (\p -> (1, p, p)) sequencePatterns) rng
 where
  sourcePattern = mkDefaultPattern Chord Do Major
  pats = neighbourChords sourcePattern
  sequencePatterns = (sourcePattern : (intersperse sourcePattern pats)) ++ [sourcePattern]

playModes :: [(Int, NotesPattern AnyOffset, NotesPattern AnyOffset)] -> GenIO -> IO PlayResult
playModes [] _ = return $ Right ()
playModes (m:ms) rng = do
  playMode m rng >>= either (return . Left) (\_ -> playModes ms rng)

playMode :: (Int, NotesPattern AnyOffset, NotesPattern AnyOffset) -> GenIO -> IO PlayResult
playMode (countBars, leftPattern, rightPattern) rng = do
  putStrLn $ show (countBars, prettyShow leftPattern, prettyShow rightPattern)
  left <- pickRandomInstructionsWeighted rng countInstructions $ fst insns
  right <- pickRandomInstructionsWeighted rng countInstructions $ snd insns
  playLoop rng (countLoops * countInstructions) left right
 where
  countLoops = countBars
  countInstructions = 32
  rangeNotes = map MidiPitch [55..71]

  insns = buildVoices leftPattern rightPattern rangeNotes

stressTest :: IO PlayResult
stressTest = playVoicesAtTempo 10000 simpleInstrument $ allCentered $ map (take 1000 . cycle) [voices|
  sol  - .  . .  .   la - .  . si -   -  - .
  vsol - -  - .  vla -  - -  . .  vsi -  . .
  do   . do . do .   do . do . do .   do - .
  |]
