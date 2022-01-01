
module Imj.Music.Random(
          Melody(..),
          computeMelody,
          mkBrownian,
          BrownianInstruction(..),
          weightedNotesUsingPattern,
          pickRandom,
          pickRandomWeighted,
          pickRandomInstructionsWeighted,
          pickBrownianInstructionsWeighted)
  where

import           System.Random.MWC(uniform, Variate(..), GenIO)

import           Data.List(foldl')
import           Data.IntSet(IntSet)
import qualified Data.IntSet as ISet

import           Imj.Prelude
import           Imj.Music.Instruction
import           Imj.Music.Harmony

data Melody = Brownian { brownianState :: Maybe (Move, MidiPitch)
                       , brownianInstructionsWeighted :: [(BrownianInstruction, Float)] }
            -- ^ with initial state
            | Random

mkBrownian :: [(BrownianInstruction, Float)] -> Melody
mkBrownian = Brownian Nothing

computeMelody :: GenIO
             -> Melody
             -> Int
               -- ^ count instructions
             -> Float
             -- ^ Rest weight (Values in [0..3] are interesting)
             -> [MidiPitch]
             -> NotesPattern AnyOffset
             -> IO ([Instruction], Melody)
computeMelody rng melo countInstructions restWeight rangeNotesMelody scale = case melo of
  Random ->
    flip (,) melo <$> pickRandomInstructionsWeighted rng countInstructions (weightedNotesUsingPattern scale rangeNotesMelody 5 0 0 restWeight)
  Brownian state brownianInsns -> (\(a, b) -> (a, Brownian (Just b) brownianInsns)) <$>
    pickBrownianInstructionsWeighted rng countInstructions state (pitchesInPattern scale rangeNotesMelody) brownianInsns


pitchesInPattern :: NotesPattern AnyOffset -> [MidiPitch] -> IntSet
pitchesInPattern pattern allNotes =
  ISet.fromList $ -- we could use fromAscList if we assume that allNotes is ascending
    map fromIntegral $ filter (inPattern pattern) allNotes

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

pickRandom :: GenIO -> Int -> [a] -> IO [a]
pickRandom rng count l = do
  ints <- replicateM count $ uniform rng :: IO [Int]
  return $ map (((!!) l) . idx) ints
 where
  idx i = mod i m
  m = length l

pickOneRandom :: GenIO -> [a] -> IO a
pickOneRandom rng l = pickRandom rng 1 l >>= \res -> case res of
  (r:[]) -> return r
  _ -> error "logic"

pickRandomWeighted :: GenIO -> Int -> [(a, Float)] -> IO [a]
pickRandomWeighted rng count l = do
  replicateM count pick
 where
  pick = do
    word <- uniform rng :: IO Word
    return $ accWeightToValue 0 l $ intToAccWeight word
  intToAccWeight i = sumWeights * ((fromIntegral i) / (fromIntegral (maxBound :: Word)))
  sumWeights = foldl' (\acc (_, w) -> acc + w) 0 l
  accWeightToValue _ [] _ = error "not found"
  accWeightToValue accWeight ((val, weight):es) aw =
    if (accWeight + weight >= aw)
      then val
      else accWeightToValue (accWeight + weight) es aw

{--
  Ensures Instructions are coherent, i.e
 - we use Extend only if a note is being played
--}
sanitizeInstructions :: [Instruction] -> [Instruction]
sanitizeInstructions insns = sanitize Nothing [] insns
 where
  sanitize mayPrev acc remain = case remain of
    [] -> reverse acc
    (r:nextRemain) ->
      sanitize
      (Just r)
      ((maybe
        (case r of
          Extend -> Rest
          _ -> r)
        (\prev -> case r of
          Extend -> (case prev of
            Rest -> Rest
            _ -> Extend)
          _ -> r)
        mayPrev): acc)
      nextRemain

pickRandomInstructionsWeighted :: GenIO -> Int -> [(Instruction, Float)] -> IO [Instruction]
pickRandomInstructionsWeighted rng count l = do
  insns <- pickRandomWeighted rng count l
  return $ sanitizeInstructions insns

data Move = Up | Down
  deriving(Show)

changeDirection :: Move -> Move
changeDirection Up = Down
changeDirection Down = Up

data BrownianInstruction = Imperative !Instruction
                         | MonotonicMove
                         | NonMonotonicMove
                         | Repetition
  deriving(Show)

findNextPitchMayRebound :: (Move, MidiPitch) -> IntSet -> (Move, MidiPitch)
findNextPitchMayRebound (curMove, curPitch) allowedPitches =
  let lookupMove m = case m of
        Up -> ISet.lookupLT
        Down -> ISet.lookupGT
      otherMove = changeDirection curMove

  in maybe
      (maybe
        (curMove, curPitch) -- allowedPitches has one or less elements
        (\newPitch -> (otherMove, fromIntegral newPitch))
        $ lookupMove otherMove (fromIntegral curPitch) allowedPitches) -- rebound
      (\newPitch -> (curMove, fromIntegral newPitch))
      $ lookupMove curMove (fromIntegral curPitch) allowedPitches

nextBrownian :: (Move, MidiPitch) -> IntSet -> BrownianInstruction -> (Instruction, (Move, MidiPitch))
nextBrownian (prevMove, prevPitch) allowedPitches i = case i of
  (Imperative note@(Note name oct)) ->
    (note, (prevMove, noteToMidiPitch name oct))
  (Imperative Extend) ->
    (Extend, (prevMove, prevPitch))
  (Imperative Rest) ->
    (Rest, (prevMove, prevPitch))
  Repetition -> (uncurry Note $ midiPitchToNoteAndOctave prevPitch, (prevMove, prevPitch))
  MonotonicMove -> move prevMove
  NonMonotonicMove -> move $ changeDirection prevMove
 where
  move dir =
    let (mv, pitch) = findNextPitchMayRebound (dir, prevPitch) allowedPitches
    in (uncurry Note $ midiPitchToNoteAndOctave pitch, (mv, pitch))

pickBrownianInstructionsWeighted :: GenIO -> Int -> Maybe (Move, MidiPitch) -> IntSet -> [(BrownianInstruction, Float)] -> IO ([Instruction], (Move, MidiPitch))
pickBrownianInstructionsWeighted rng count mayState allowedPitches l = do
  -- TODO try to increase temporal coherence (short term stability) by making the probabilities vary by time.
  -- for example, if we picked MonotonicMove, we should make it probable that we will do the same in the next few iterations
  -- but improbable after that
  (prevMove_, prevPitch_) <- maybe
    (do
      mv <- pickOneRandom rng [Up, Down]
      pit <- pickOneRandom rng $ ISet.toList allowedPitches
      return (mv, fromIntegral pit)
    )
    return
    mayState
  brownianInsns <- pickRandomWeighted rng count l
  return $ (\(a, b) -> (sanitize prevPitch_ a, b)) $ go brownianInsns [] (prevMove_, prevPitch_)
 where
  go [] res prevPitchMove = (reverse res, prevPitchMove)
  go (r:remaining) res prevPitchMove =
    let (insn, pitchMove) = nextBrownian prevPitchMove allowedPitches r
    in go remaining (insn:res) pitchMove

  sanitize _ [] = []
  sanitize prevPitch (r:rs) = (case r of
    -- the first instruction is an extend so we convert it to a note
    Extend -> uncurry Note (midiPitchToNoteAndOctave prevPitch)
    Rest -> r
    Note{} -> r):rs
