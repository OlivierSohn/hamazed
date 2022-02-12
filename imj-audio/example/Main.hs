
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent(threadDelay)
import           Control.Monad(void)

import           System.Random.MWC(create, GenIO)
import           Data.Bool(bool)
import           Data.List(intersperse, splitAt)
import qualified Data.HashSet as HashSet

import           Imj.Audio hiding (intersperse, transpose)
import           Imj.Music.Random
import           Imj.Music.Harmony
import           Imj.Music.Compositions.Me
import           Imj.Music.Compositions.Tech
import           Imj.Music.Compositions.Tchaikovski
import           Imj.Music.Compositions.Vivaldi

playShortLowNote :: Instrument -> VoiceId -> IO (Either () ())
playShortLowNote instrument v = do
  play (StartNote Nothing (InstrumentNote Do (Octave 6) instrument) (NoteVelocity 0.01) panCentered) v
    >>= either (return . Left)
    (\_ -> play (StopNote Nothing (InstrumentNote Do (Octave 6) instrument)) v)

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
  playShortLowNote meSnare (VoiceId 0) >>= print
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

noteInsnsForPattern :: NotesPattern AnyOffset -> [MidiPitch] -> [Instruction]
noteInsnsForPattern pattern allNotes =
  map
    (\pitch -> uncurry Note $ midiPitchToNoteAndOctave pitch)
    $ filter (inPattern pattern) allNotes

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
playRandomScore =
  create >>= playModes (concat $ take 2 $ repeat patterns)
 where
  patterns = map (\(n, note, mode) -> (n, mkDefaultPattern Chord note mode, mkDefaultPattern Chord note mode)) modes
  modes =
    [ (1, Do, Major)
    , (1, Mi, Major)
    , (1, La, Minor)
    , (1, Sol, Major)]

playNeighbourPatterns :: IO PlayResult
playNeighbourPatterns = do
  mapM_(\(n, p) -> putStrLn $ (show n) ++ " " ++ (prettyShow p)) pats
  create >>= playModes (map (\p -> (1, p, p)) sequencePatterns)
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
  rng <- create
  go rng 25 (mkKey Do majorScale) melody
 where
  rangeNotesMelody = map MidiPitch [(75-11)..75]
  tempo = 600
  countLoopsRegul = 1
  countLoopsPivot = 1
  countInstructionsRegul = 24
  countInstructionsPivot = 8
  melodyPace = 2
  restWeightRegular = 2
  restWeightPivot = 2

  melody = mkBrownian [
      (Repetition, 0)
    , (MonotonicMove, 4)
    , (NonMonotonicMove, 1)
    , (Imperative Rest, 0)
    , (Imperative Extend, 2)]
  --melody = Random

  go _ 0 _ _ = return $ Right ()
  go rng n key melo = do
    return melo >>=
      playOneKey rng tempo countLoopsRegul countInstructionsRegul melodyPace restWeightRegular rangeNotesMelody key
      >>= either (return . Left)
      (playPivotAndAccidental rng tempo countLoopsPivot countInstructionsPivot melodyPace restWeightPivot rangeNotesMelody key nextKey)
      >>= either (return . Left)
      (go rng (pred n) nextKey)
   where
     nextKey = bool (mkFifthsShiftedKey 1 key) (mkRelativeKey key) $ even n

playPivotAndAccidental :: GenIO
                       -> Double
                       -- ^ Tempo
                       -> Int
                       -- ^ count loops
                       -> Int
                       -- ^ count instructions
                       -> Int
                       -- ^ melody pace
                       -> Float
                       -- ^ Rest weight (Values in [0..3] are interesting)
                       -> [MidiPitch]
                       -> Key
                       -> Key
                       -> Melody
                       -> IO (Either () Melody)
playPivotAndAccidental rng tempo countLoops countInstructions melodyPace restWeight rangeNotesMelody key nextKey@(Key targetScale _ _) melo = do
  putStrLn $ "pivot from: " ++ prettyShowKey key
  putStrLn $ "pivot to  : " ++ prettyShowKey nextKey
  -- play the accidental as a passing note.
  (melody, newMelo) <- computeMelody rng melo countInstructions restWeight rangeNotesMelody targetScale
  -- harmonize using _only_ the pivot chord
  harmony <- harmonize rng triadFromPitch melodyPace melody 4 0 0 0
  mapM_ (putStrLn . show) $ zip (map show melody) $ groupShow melodyPace harmony []
  pedal <- pickRandomWeighted rng countSteps [(True, 0), (False, 1)]
  either Left (const $ Right newMelo) <$> playLoop tempo countSteps harmony (slowDown melodyPace melody) pedal
 where
  pivots = HashSet.toList $ findPivotTriads key nextKey
  --accidentals = findAccidentals key nextKey

  countSteps = countLoops * countInstructions * melodyPace

  triadFromPitch mayPrevTriad pitch = case findTriadsUsingNote pivots pitch of
    (firstTriad:_) -> firstTriad
    [] -> maybe
           (case pivots of
             (firstPivot:_) -> firstPivot
             [] -> error "no triad found")
           id
           mayPrevTriad

playOneKey :: GenIO
           -> Double
           -- ^ Tempo
           -> Int
           -- ^ count loops
           -> Int
           -- ^ count instructions
           -> Int
           -- ^ melody pace

           -> Float
           -- ^ Rest weight (Values in [0..3] are interesting)
           -> [MidiPitch]
           -> Key
           -> Melody
           -> IO (Either () Melody)
playOneKey rng tempo countLoops countInstructions melodyPace restWeight rangeNotesMelody key@(Key kScale kTriads _) melo = do
  putStrLn $ prettyShowKey key
  (melody, newMelo) <- computeMelody rng melo countInstructions restWeight rangeNotesMelody kScale
  harmony <- harmonize rng triadFromPitch melodyPace melody 4 0 0 0
  mapM_ (putStrLn . show) $ zip (map show melody) $ groupShow melodyPace harmony []
  pedal <- pickRandomWeighted rng countSteps [(True, 0), (False, 1)]
  either Left (const $ Right newMelo) <$> playLoop tempo countSteps harmony (slowDown melodyPace melody) pedal
 where
  -- the melody can be slower than the arpegiated chords
  countSteps = countLoops * countInstructions * melodyPace

  triadFromPitch _ pitch = case findTriadsUsingNote kTriads pitch of
    -- TODO Add a boolean parameter to randomize the triad we pick
    -- TODO Add a boolean parameter to prioritize triads that have not been used yet
    (firstTriad:_) -> firstTriad
    [] -> error "no triad found" -- not supposed to happen, if the note is in the scale of the key

groupShow :: Show a => Int -> [a] -> [String] -> [String]
groupShow _ [] res = reverse res
groupShow sz l@(_:_) res =
  let (l1, l2) = splitAt sz l
  in groupShow sz l2 ((show l1):res)

harmonize :: GenIO
          -> (Maybe (NotesPattern AnyOffset) -> MidiPitch -> NotesPattern AnyOffset)
          -- ^ Returns the triad to use when the melody is at a given pitch, and passes te previously used triad
          -> Int
          -> [Instruction]
          -> Float
          -> Float
          -> Float
          -> Float
          -> IO [Instruction]
harmonize rng triadForMelodyPitch notesPerMelodyStep melody sumWeightsPattern sumWeightsNotPattern weightExtend weightRest = go melody Nothing Nothing []
 where
  go [] _ _ res = return $ reverse res
  go (r:remaining) mayPrevTriad mayPrevNote res = case r of
    Rest -> proceed Nothing -- TODO Add a probability parameter to _randomly_ use mayPrevNote here.
    Extend -> proceed mayPrevNote
    note@Note{} -> proceed (Just note)
   where
    proceed = maybe
      (go remaining mayPrevTriad Nothing $ (take notesPerMelodyStep $ repeat Rest) ++ res)
      (\note@(Note name octave) ->
        let pitch = noteToMidiPitch name octave
            -- Using a chord range of one octave
            -- TODO Add a boolean parameter to make the melody and the chord range move in opposite directions ()
            rangeNotesChord = [pitch - 12..pitch - 1]
            triad = triadForMelodyPitch mayPrevTriad pitch
            chordWeightedInsns =
                -- pick 'notesPerMelodyStep' notes in the triad and make sure these notes are strictly below the melody
                weightedNotesUsingPattern triad rangeNotesChord sumWeightsPattern sumWeightsNotPattern weightExtend weightRest
        in do
          chordInsns <- pickRandomInstructionsWeighted rng notesPerMelodyStep chordWeightedInsns
          go remaining (Just triad) (Just note) $ (reverse chordInsns) ++ res)

stressTest :: IO PlayResult
stressTest = playVoicesAtTempo 10000 simpleInstrument $ allCentered $ map (take 1000 . cycle) [voices|
  sol  - .  . .  .   la - .  . si -   -  - .
  vsol - -  - .  vla -  - -  . .  vsi -  . .
  do   . do . do .   do . do . do .   do - .
  |]
