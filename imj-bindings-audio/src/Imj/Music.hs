{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imj.Music
      ( Symbol(..)
      , NoteSpec(..)
      , musicSymbol
      , musicSymbols
      , NoteName(..)
      , Octave(..)
      , noOctave
      , MidiVelocity(..)
      , Tempo(..)
      , Score
      , sizeScore
      , notes
      , mkScore
      , stepScore
      , stopScore
      , stepNScore
      , stepNScoreAndStop
      , Music(..)
      , play
      ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Control.DeepSeq (NFData(..))
import           Data.Binary
import           Data.Data(Data(..))
import           Data.Maybe(catMaybes, maybeToList)
import           Data.Text(pack)
import qualified Data.Vector as V
import           Foreign.C
import           GHC.Generics (Generic)
import           Text.Parsec(parse, (<|>), char, anyChar, spaces, space, eof, many1, choice, manyTill
                            , SourcePos, setPosition)
import           Text.Parsec.Text(Parser)
import           Text.Parsec.Pos(newPos)

import           Imj.Audio

noOctave :: Octave
noOctave = Octave 6

data Symbol =
    Note {-# UNPACK #-} !NoteSpec
  | Extend
  | Rest
  deriving(Generic,Show, Eq, Data)
instance Binary Symbol
instance NFData Symbol

data NoteSpec = NoteSpec !NoteName {-# UNPACK #-} !Octave
  deriving(Generic,Show, Eq, Data)
instance Binary NoteSpec
instance NFData NoteSpec

musicSymbol :: Parser Symbol
musicSymbol = do
  spaces
  choice
    [ rest
    , prolong
    , note 0
    ]
 where
  rest = do
    _ <- char '.'
    spaces
    return Rest

  prolong = do
    _ <- char '-'
    spaces
    return Extend

  lower = do
    _ <- char 'v'
    spaces
    return $ -1

  upper = do
    _ <- char '^'
    spaces
    return 1

  note n = choice
    [ lower >>= note . (+) n
    , upper >>= note . (+) n
    , go n
    ]
   where
     go x = do
      noteName <- manyTill anyChar (eof <|> (do _ <- space; return ())) >>= \case
        "do"   -> return Do
        "ré"   -> return Ré
        "mi"   -> return Mi
        "fa"   -> return Fa
        "sol"  -> return Sol
        "la"   -> return La
        "si"   -> return Si
        "réb"  -> return Réb
        "mib"  -> return Mib
        "solb" -> return Solb
        "lab"  -> return Lab
        "sib"  -> return Sib
        str -> fail $"Wrong note:" ++ str
      spaces
      return $ Note $ NoteSpec noteName $ x + noOctave

musicSymbols :: Parser [Symbol]
musicSymbols = many1 musicSymbol

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

topLevel :: Parser a -> Parser a
topLevel p = spaces *> p <* eof

notes :: QuasiQuoter
notes = QuasiQuoter {
      quoteExp = \str -> do
        l <- location'
        let c' = parse (setPosition l *> topLevel musicSymbols) "" $ pack str
        c <- either (error.show) return c'
        dataToExpQ (const Nothing) c
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

-- | Keeps track of progress, and loops when the end is found.
data Score = Score {
    _nextIdx :: !NoteIdx
  , _curNote :: (Maybe Symbol)
  -- ^ The invariant is that this can never be 'Just' 'Extend' : instead,
  -- when a 'Extend' symbol is encountered, we don't change this value.
  , _noteSequence :: !(V.Vector Symbol)
} deriving(Generic,Show, Eq)

mkScore :: [Symbol] -> Score
mkScore l = Score 0 Nothing $ V.fromList l

sizeScore :: Score -> Int
sizeScore (Score _ _ v) = V.length v

-- | Like 'stepNScore' but also uses 'stopScore' to finalize the music.
stepNScoreAndStop :: Int -> Score -> (Score, [[Music]])
stepNScoreAndStop n s =
  (s'', reverse $ lastMusic:music)
 where
  (s', music) = stepNScoreReversed n s
  (s'', lastMusic) = stopScore s'

stepNScoreReversed :: Int -> Score -> (Score, [[Music]])
stepNScoreReversed n score
  | n < 0 = (score,[])
  | otherwise = go n score []
 where
  go 0 s l = (s, l)
  go i s l = let (s',m) = stepScore s in go (i-1) s' $ m:l

stepNScore :: Int -> Score -> (Score, [[Music]])
stepNScore n score = let (s,l) = stepNScoreReversed n score in (s,reverse l)

stepScore :: Score
          -> (Score, [Music])
          -- ^ returns the (Maybe) previous note and the current note
stepScore (Score (NoteIdx i) cur v) =
    ( Score nextI newCur v
    , catMaybes [mayStopCur, mayStartNext])
 where
  nextNote = v V.! i

  newCur = case nextNote of
    Extend -> cur
    _ -> Just nextNote

  mayStopCur =
    maybe
      Nothing
      (\case
        Rest -> Nothing
        Extend -> error "logic"
        (Note n) -> case nextNote of
          Extend -> Nothing
          _ -> Just $ StopNote n)
      cur

  mayStartNext = case nextNote of
    (Note n) -> Just $ StartNote n 1
    _ -> Nothing

  len = V.length v

  nextI
    | i < len-1 = fromIntegral $ i+1
    | otherwise = 0

stopScore :: Score -> (Score, [Music])
stopScore (Score _ cur l) =
    ( Score 0 Nothing l
    , maybeToList noteChange)
 where
  noteChange = maybe Nothing (\case
    Rest -> Nothing
    Note n -> Just $ StopNote n
    Extend -> error "logic") cur

-- | A music fragment
data Music =
     StartNote !NoteSpec {-# UNPACK #-} !MidiVelocity
   | StopNote !NoteSpec
  deriving(Generic,Show, Eq)
instance Binary Music
instance NFData Music



play :: Music -> IO ()
play (StartNote n v) =
  noteOn n v
play (StopNote n) =
  noteOff n


newtype NoteIdx = NoteIdx Int
  deriving(Generic,Show, Num, Integral, Real, Ord, Eq, Enum)

newtype Tempo = Tempo Float -- in beats per second
  deriving(Generic,Show,Eq)

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
  deriving(Generic,Show,Eq, Data)
instance Binary NoteName
instance NFData NoteName

newtype Octave = Octave Int
  deriving(Generic,Integral, Real, Num, Enum, Ord, Eq,Show,Binary,NFData, Data)

newtype MidiVelocity = MidiVelocity Float
 deriving (Generic, Num,Show,Eq)
instance Binary MidiVelocity
instance NFData MidiVelocity


-- according to http://subsynth.sourceforge.net/midinote2freq.html, C1 has 0 pitch
noteToMidiPitch :: NoteSpec -> CInt
noteToMidiPitch (NoteSpec n oct) = 12 * (fromIntegral oct-1) + noteIdx n

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

noteOn :: NoteSpec -> MidiVelocity -> IO ()
noteOn note (MidiVelocity vel) = midiNoteOn (noteToMidiPitch note) $ CFloat vel

noteOff :: NoteSpec -> IO ()
noteOff note = midiNoteOff $ noteToMidiPitch note
