
module Imj.Music.Harmony
      ( -- | Standard scales
        majorScale
      , minorNaturalScale
      , minorMelodicScale
      , minorHarmonicScale
      -- | Chords
      , majorChord
      , minorChord
      -- | Keys
      , Key(..)
      , mkKey
      , prettyShowKey
      , findTriadsUsingNote
      , findPivotTriads
      , findAccidentals
      -- | Utilities
      , Mode(..)
      , Pattern(..)
      , AnyOffset
      , patternUnion
      , prettyShow
      , inPattern
      , NotesPattern(..)
      , mkDefaultPattern
      , neighbourChords
      , neighbourPatterns
      , countCommonNotes
      , transpose
      -- | For tests only
      , shiftModDescPatternToAscList
      ) where

import Imj.Music.Instruction

import           Data.IntSet(IntSet)
import qualified Data.IntSet as ISet
import           Data.HashSet(HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable
import           Data.Bool(bool)
import           Data.List(sortOn, intersperse)

data Key = Key {
    keyScale :: !(NotesPattern AnyOffset)
  , keyTriadsFromRootAscending :: ![NotesPattern AnyOffset]
  , keyTriadsLookup :: !(HashSet (NotesPattern AnyOffset))
}
  deriving(Show)

mkKey :: NoteName -> NotesPattern ZeroOffset -> Key
mkKey n z = Key p tr trl
 where
  p = transpose (fromEnum n) z
  tr = map (transpose (fromEnum n)) $ mkTriads z
  trl = HashSet.fromList tr

mkTriads :: NotesPattern ZeroOffset -> [NotesPattern AnyOffset]
mkTriads (NotesPattern p) =
  map
    (\i -> NotesPattern $ ISet.fromList $ map ((!!) listNotes) [i, mod (i+2) sz, mod (i+4) sz])
    [0..(sz-1)]
 where
  listNotes = ISet.toAscList p
  sz = length listNotes

prettyShowKey :: Key -> String
prettyShowKey (Key scale tris _) = "key:" ++ prettyShow scale ++ ", triads:" ++ concat (intersperse "," $ map prettyShow tris)

-- | Returns triads that contain the played note.
-- The returned list contains at least one element if the played note is in the scale of the key.
findTriadsUsingNote :: [NotesPattern AnyOffset] -> MidiPitch -> [NotesPattern AnyOffset]
findTriadsUsingNote triads playedNote =
  filter (flip inPattern playedNote) triads

-- | Returns triads that are common to both keys
findPivotTriads :: Key -> Key -> HashSet (NotesPattern AnyOffset)
findPivotTriads (Key _ _ t1) (Key _ _ t2) = HashSet.intersection t1 t2

-- | Returns notes that are in destintaion key but not in origin key
findAccidentals :: Key -> Key -> NotesPattern AnyOffset
findAccidentals (Key (NotesPattern scaleOrigin) _ _) (Key (NotesPattern scaleDestination) _ _) =
  NotesPattern $ ISet.difference scaleDestination scaleOrigin

-- Invariant : values are homogenous to a MidiPitch modulo 12, hence are in [0, 12) (we could use a bit set instead)
-- hence 0 corresponds to Do
newtype NotesPattern a = NotesPattern IntSet
  deriving(Show, Eq)

data ZeroOffset
data AnyOffset

instance Hashable (NotesPattern a) where
  hashWithSalt = hashUsing (ISet.toAscList . (\(NotesPattern is) -> is))

patternUnion :: NotesPattern AnyOffset
             -> NotesPattern AnyOffset
             -> NotesPattern AnyOffset
patternUnion (NotesPattern p1) (NotesPattern p2) =
  NotesPattern $ ISet.union p1 p2

data Mode = Major | Minor
  deriving(Show, Eq)
data Pattern = Chord | Scale
  deriving(Show, Eq)

mkDefaultPattern :: Pattern -> NoteName -> Mode -> NotesPattern AnyOffset
mkDefaultPattern p note m = transpose (noteNameToMidiModuloPitch note) (zeroPitchPattern m p)

zeroPitchPattern :: Mode -> Pattern -> NotesPattern ZeroOffset
zeroPitchPattern Major Scale = majorScale
zeroPitchPattern Minor Scale = minorNaturalScale
zeroPitchPattern Major Chord = majorChord
zeroPitchPattern Minor Chord = minorChord

-- There are 12 half tones per octave
semiTonesPerOctave :: Int
semiTonesPerOctave = 12

majorScale :: NotesPattern ZeroOffset
majorScale = NotesPattern $ ISet.fromDistinctAscList [0, 2, 4, 5, 7, 9, 11]

minorNaturalScale :: NotesPattern ZeroOffset
minorNaturalScale = NotesPattern $ ISet.fromDistinctAscList [0, 2, 3, 5, 7, 8, 10]

minorHarmonicScale :: NotesPattern ZeroOffset
minorHarmonicScale = NotesPattern $ ISet.fromDistinctAscList [0, 2, 3, 5, 7, 8, 11]

majorChord :: NotesPattern ZeroOffset
majorChord = NotesPattern $ ISet.fromDistinctAscList [0, 4, 7]

minorChord :: NotesPattern ZeroOffset
minorChord = NotesPattern $ ISet.fromDistinctAscList [0, 3, 7]

-- This is the ascending part of the minor melodic scale.
-- The descending part is the minor natural scale.
minorMelodicScale :: NotesPattern ZeroOffset
minorMelodicScale = NotesPattern $ ISet.fromAscList [0, 2, 3, 5, 7, 9, 11]

allZeroPitchChords :: [(NotesPattern ZeroOffset, [Char])]
allZeroPitchScales :: [(NotesPattern ZeroOffset, [Char])]
allZeroPitch :: [(NotesPattern ZeroOffset, [Char])]
allZeroPitchChords = [
    (majorChord, "M")
  , (minorChord, "m")
  ]
allZeroPitchScales = [
    (majorScale, "Major")
  , (minorMelodicScale, "minor melodic")
  , (minorHarmonicScale, "minor harmonic")
  , (minorNaturalScale, "minor natural")
  ]
allZeroPitch = allZeroPitchChords ++ allZeroPitchScales

-- | Gets the offset wrt one potential ZeroOffset pattern
matchPatternByOffset :: NotesPattern ZeroOffset
                     -> NotesPattern AnyOffset
                     -> Maybe NoteName
matchPatternByOffset (NotesPattern p0) pat@(NotesPattern p)
  | ISet.size p0 /= ISet.size p = Nothing
  | ISet.size p0 == 0 = Nothing
  | otherwise = go offsets
 where
  zero = ISet.findMin p0
  offsets = map (\pIdx -> zero - pIdx) $ ISet.toAscList p
  go [] = Nothing
  go (offset:os) =
    let candidate = transpose offset pat
    in case candidate of
      (NotesPattern ca) -> bool (go os) (Just $ toEnum $ mod (-offset) semiTonesPerOctave) $ ca == p0

match :: NotesPattern AnyOffset -> Maybe (NoteName, String)
match p = go allZeroPitch
 where
  go [] = Nothing
  go ((p0, name):p0s) = maybe (go p0s) (\rootNote -> Just (rootNote, name)) $ matchPatternByOffset p0 p

prettyShow :: NotesPattern AnyOffset -> String
prettyShow p = maybe
  (show p)
  (\(note, name) -> (show note) ++ " " ++ name)
  $ match p

modPitch :: Int -> Int
modPitch pitch =
  let pitchMod_ = mod pitch semiTonesPerOctave
  in if pitchMod_ < 0 then pitchMod_ + semiTonesPerOctave else pitchMod_

inPattern :: NotesPattern AnyOffset -> MidiPitch -> Bool
inPattern (NotesPattern pattern) (MidiPitch pitch) =
  ISet.member pitchMod pattern
 where
  pitchMod = modPitch $ fromIntegral pitch

-- Precondition: the input list is piecewise descending by (at most 2) pieces,
-- and the second piece has values greater than the first piece.
shiftModDescPatternToAscList :: [Int] -> [Int]
shiftModDescPatternToAscList [] = [] -- should never happen
shiftModDescPatternToAscList (i:is) =
  go is [i]
 where
  go (_:_) [] = error "logic"
  go l@(j:js) l1@(k:_)
    | j < k = go js (j:l1)
    | otherwise = l1 ++ (reverse l)
  go [] l1 = l1

transpose :: Int -> NotesPattern a -> NotesPattern AnyOffset
transpose offset (NotesPattern p) =
  NotesPattern $ ISet.fromDistinctAscList $ shiftModDescPatternToAscList $
    map
      (modPitch . ((+) offset)) $
      ISet.toDescList p

-- | Neighbours share one or more notes
neighbourPatterns :: NotesPattern AnyOffset -> NotesPattern ZeroOffset -> HashSet (NotesPattern AnyOffset)
neighbourPatterns s@(NotesPattern sourceChord) dest@(NotesPattern destinationChord) =
  -- each note of the source pattern can be each note of the destination pattern
  HashSet.delete s $
  HashSet.fromList $ concatMap
    (\sourceNote -> map
                      (\destNote -> transpose (sourceNote - destNote) dest
                      ) $ ISet.toDescList destinationChord
    ) $ ISet.toDescList sourceChord

neighbourChords :: NotesPattern AnyOffset -> [(Int, NotesPattern AnyOffset)]
neighbourChords pattern = sortOn (negate . fst) $ map (\p -> (countCommonNotes pattern p, p)) pats
  where
    -- by construction, neighbour patterns using majorChord are different from neighbourgh patterns using minorChord
    -- so we don't need to use a set for the union
    pats = concatMap (HashSet.toList . (neighbourPatterns pattern)) [majorChord, minorChord]

countCommonNotes :: NotesPattern AnyOffset -> NotesPattern AnyOffset -> Int
countCommonNotes (NotesPattern a) (NotesPattern b) = ISet.size $ ISet.intersection a b
