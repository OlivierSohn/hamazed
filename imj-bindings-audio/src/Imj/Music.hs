{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Music
      ( Note(..)
      , NoteName(..)
      , Octave(..)
      , NoteVelocity(..)
      , Tempo(..)
      , Score
      , mkScore
      , stepScore
      , stopScore
      , Music(..)
      , play
      ) where

import Control.DeepSeq (NFData(..))
import Data.Binary
import GHC.Generics (Generic)
import Foreign.C
import Data.Vector as V

import Imj.Audio

data Note = Note !NoteName {-# UNPACK #-} !Octave
  deriving(Generic,Show)
instance Binary Note
instance NFData Note

-- | Keeps track of progress, and loops.
data Score = Score {
    _nextNote :: !NoteIdx
  , _curNote :: (Maybe Note)
  , _tempo :: !(Maybe Tempo)
  , _noteSequence :: !(V.Vector Note)
} deriving(Generic,Show)

mkScore :: Maybe Tempo -> [Note] -> Score
mkScore t l = Score 0 Nothing t $ V.fromList l

stepScore :: Score
          -> (Score, Music)
          -- ^ returns the (Maybe) previous note and the current note
stepScore (Score (NoteIdx i) cur t l) =
    (Score nextI (Just nextNote) t l
    ,noteChange)
 where
  noteChange =
    maybe
      (StartNote nextNote 1)
      (\c -> ReplaceNote c nextNote 1)
      cur

  len = V.length l

  nextNote = l V.! i

  nextI
    | i < len-1 = fromIntegral $ i+1
    | otherwise = 0

stopScore :: Score -> (Score, Maybe Music)
stopScore (Score _ cur t l) =
    (Score 0 Nothing t l
    ,noteChange)
 where
  noteChange =
    fmap StopNote cur

-- | A music fragment
data Music =
     StartNote !Note {-# UNPACK #-} !NoteVelocity
   | ReplaceNote !Note !Note {-# UNPACK #-} !NoteVelocity -- Today, this is like doing StopNote, then StartNote
   | StopNote !Note
  deriving(Generic,Show)
instance Binary Music
instance NFData Music

play :: Music -> IO ()
play (StartNote n v) =
  noteOn n v
play (ReplaceNote n1 n2 v) = do
  noteOff n1
  noteOn  n2 v
play (StopNote n) =
  noteOff n


newtype NoteIdx = NoteIdx Int
  deriving(Generic,Show, Num, Integral, Real, Ord, Eq, Enum)

newtype Tempo = Tempo Float -- in beats per second
  deriving(Generic,Show)

data NoteName =
    Do
  | Réb
  | Ré
  | Mib
  | Mi
  | Fa
  | Solb
  | Sol
  | Lab
  | La
  | Sib
  | Si
  deriving(Generic,Show)
instance Binary NoteName
instance NFData NoteName

newtype Octave = Octave Int
  deriving(Generic,Integral, Real, Num, Enum, Ord, Eq,Show,Binary,NFData)

newtype NoteVelocity = NoteVelocity Float
 deriving (Generic, Num,Show)
instance Binary NoteVelocity
instance NFData NoteVelocity


-- according to http://subsynth.sourceforge.net/midinote2freq.html, C1 has 0 pitch
noteToMidiPitch :: Note -> CInt
noteToMidiPitch (Note n oct) = 12 * (fromIntegral oct-1) + noteIdx n

noteIdx :: NoteName -> CInt
noteIdx Do = 0
noteIdx Réb = 1
noteIdx Ré = 2
noteIdx Mib = 3
noteIdx Mi = 4
noteIdx Fa = 5
noteIdx Solb = 6
noteIdx Sol = 7
noteIdx Lab = 8
noteIdx La = 9
noteIdx Sib = 10
noteIdx Si = 11

noteOn :: Note -> NoteVelocity -> IO ()
noteOn note (NoteVelocity vel) = midiNoteOn (noteToMidiPitch note) $ CFloat vel

noteOff :: Note -> IO ()
noteOff note = midiNoteOff $ noteToMidiPitch note
