
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent(threadDelay)
import           Control.Monad(void)

import           System.Random.MWC(create, GenIO)
import           Data.Bool(bool)
import           Data.List(intersperse, splitAt)

import           Imj.Audio hiding (intersperse, transpose)
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
  putStrLn "playing in a key"
  playKey >>= print
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

playLoop :: Double -> Int -> [Instruction] -> [Instruction] -> [Bool] -> IO PlayResult
playLoop tempo countSteps leftInsns rightInsns pedal =
  playVoicesAtTempoPedal tempo simpleInstrument
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
  _ <- mapM (\(n, p) -> putStrLn $ (show n) ++ " " ++ (prettyShow p)) pats
  playModes (map (\p -> (1, p, p)) sequencePatterns) rng
 where
  sourcePattern = mkDefaultPattern Chord Do Major
  pats = neighbourChords sourcePattern
  sequencePatterns = (sourcePattern : (intersperse sourcePattern $ map snd pats)) ++ [sourcePattern]

playModes :: [(Int, NotesPattern AnyOffset, NotesPattern AnyOffset)] -> GenIO -> IO PlayResult
playModes [] _ = return $ Right ()
playModes (m:ms) rng = do
  playMode m rng >>= either (return . Left) (\_ -> playModes ms rng)

playMode :: (Int, NotesPattern AnyOffset, NotesPattern AnyOffset) -> GenIO -> IO PlayResult
playMode (countBars, leftPattern, rightPattern) rng = do
  putStrLn $ show (countBars, prettyShow leftPattern, prettyShow rightPattern)
  left <- pickRandomInstructionsWeighted rng countInstructions $ fst insns
  right <- pickRandomInstructionsWeighted rng countInstructions $ snd insns
  pedal <- pickRandomWeighted rng countSteps [(True, 0), (False, 1)]
  playLoop tempo countSteps left right pedal
 where
  countLoops = countBars
  countInstructions = 32
  rangeNotes = map MidiPitch [55..71]
  tempo = 440
  countSteps = countLoops * countInstructions
  insns = buildVoices leftPattern rightPattern rangeNotes


playKey :: IO PlayResult
playKey = do
  putStrLn $ prettyShowKey key
  rng <- create
  melody <- pickRandomInstructionsWeighted rng countInstructions melodyWeightedInsns
  putStrLn $ show melody
  harmony <- harmonize rng key melodyPace melody 4 0 0 0
  mapM_ (putStrLn . show) $ zip (map show melody) $ group melodyPace harmony []
  mapM_ (putStrLn . show) $ slowDown melodyPace melody
  pedal <- pickRandomWeighted rng countSteps [(True, 0), (False, 1)]
  playLoop tempo countSteps harmony (slowDown melodyPace melody) pedal
 where
  key@(Key kScale _) = mkKey Do majorScale
  rangeNotesMelody = map MidiPitch [(75-8)..75]
  melodyWeightedInsns = weightedNotesUsingPattern kScale rangeNotesMelody 5 0 0 3 -- Rest values in [0..3] are interesting
  countInstructions = 4 * 16
  countLoops = 4
  -- the melody is 4x slower than the arpegiated chords
  melodyPace = 1
  countSteps = countLoops * countInstructions * melodyPace
  tempo = 440
  group _ [] res = reverse res
  group sz l@(_:_) res =
    let (l1, l2) = splitAt sz l
    in group sz l2 ((show l1):res)

harmonize :: GenIO
          -> Key
          -> Int
          -> [Instruction]
          -> Float
          -> Float
          -> Float
          -> Float
          -> IO [Instruction]
harmonize rng key notesPerMelodyStep melody sumWeightsPattern sumWeightsNotPattern weightExtend weightRest = go melody Nothing []
 where
  go [] _ res = return $ reverse res
  go (r:remaining) mayPrevNote res = case r of
    Rest -> proceed Nothing -- TODO or mayPrevNote, randomly
    Extend -> proceed mayPrevNote
    note@Note{} -> proceed $ Just note
   where
    proceed = maybe
      (go remaining Nothing $ (take notesPerMelodyStep $ repeat Rest) ++ res)
      (\note@(Note name octave) ->
        let pitch = noteToMidiPitch name octave
            -- Using a chord range of one octave
            -- TODO make the melody and the chord range move in opposite directions
            rangeNotesChord = [pitch - 12..pitch - 1]
            chordWeightedInsns = case matchingTriads key pitch of
              -- TODO randomize the triad we pick
              -- TODO keep a history of recently used triads and prefer using new ones
              (firstTriad:_) ->
                -- pick 'notesPerMelodyStep' notes in the triad and make sure these notes are strictly below the melody
                weightedNotesUsingPattern firstTriad rangeNotesChord sumWeightsPattern sumWeightsNotPattern weightExtend weightRest
              [] -> error "no triad found"
        in do
          chordInsns <- pickRandomInstructionsWeighted rng notesPerMelodyStep chordWeightedInsns
          go remaining (Just note) $ (reverse chordInsns) ++ res)

stressTest :: IO PlayResult
stressTest = playVoicesAtTempo 10000 simpleInstrument $ allCentered $ map (take 1000 . cycle) [voices|
  sol  - .  . .  .   la - .  . si -   -  - .
  vsol - -  - .  vla -  - -  . .  vsi -  . .
  do   . do . do .   do . do . do .   do - .
  |]
