{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imj.Music.Play
      ( noteToMidiPitch
      , stepScore
      , stopScore
      , sizeVoice
      , stepVoice
      , stopVoice
      , stepNVoice
      , stepNVoiceAndStop
      , play
      , playAtTempo
      , allMusic
      ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay, forkIO)
import           Control.Monad(void)
import           Data.Maybe(catMaybes, maybeToList)
import qualified Data.Vector as V
import           Foreign.C

import           Imj.Audio
import           Imj.Music.Types

playAtTempo :: Float
            -- ^ BPMs
            -> Instrument
            -> [Symbol]
            -> IO ()
playAtTempo tempo i =
  void . forkIO . go . allMusic i
 where
  go [] = return()
  go (n:ns) = do
    mapM_ play n
    threadDelay pause
    go ns

  pause = round $ 1000*1000*60/tempo

-- TODO use ids for notes (one id per voice would be enough), to support this case well:
-- voice1: do - - - -
-- voice2: . . do . .
-- when the do of voice2 terminates, we want it to fadeout its own channel, not the one
-- of the other voice. NOTE it makes a difference only if the 2 notes have different velocities,
-- which is not possible as of today.
stepScore :: Score
          -> (Score, [Music])
stepScore (Score l) = (s,m)
 where
  nv = map stepVoice l
  s = Score $ map fst nv
  m = concatMap snd nv

stopScore :: Score
          -> (Score, [Music])
stopScore (Score l) = (s,m)
 where
  nv = map stopVoice l
  s = Score $ map fst nv
  m = concatMap snd nv

allMusic :: Instrument -> [Symbol] -> [[Music]]
allMusic i x =
  snd $ stepNVoiceAndStop (sizeVoice s) s
 where
  s = mkVoice i x

sizeVoice :: Voice -> Int
sizeVoice (Voice _ _ v _) = V.length v

-- | Like 'stepNVoice' but also uses 'stopVoice' to finalize the music.
stepNVoiceAndStop :: Int -> Voice -> (Voice, [[Music]])
stepNVoiceAndStop n s =
  (s'', reverse $ lastMusic:music)
 where
  (s', music) = stepNVoiceReversed n s
  (s'', lastMusic) = stopVoice s'

stepNVoiceReversed :: Int -> Voice -> (Voice, [[Music]])
stepNVoiceReversed n score
  | n < 0 = (score,[])
  | otherwise = go n score []
 where
  go 0 s l = (s, l)
  go i s l = let (s',m) = stepVoice s in go (i-1) s' $ m:l

stepNVoice :: Int -> Voice -> (Voice, [[Music]])
stepNVoice n score = let (s,l) = stepNVoiceReversed n score in (s,reverse l)

stepVoice :: Voice
          -> (Voice, [Music])
stepVoice (Voice i cur v inst) =
    ( Voice nextI newCur v inst
    , catMaybes [mayStopCur, mayStartNext])
 where
  nextNote = v V.! (fromIntegral i)

  newCur = case nextNote of
    Extend -> cur
    _ -> Just nextNote

  mayStopCur =
    maybe
      Nothing
      (\case
        Rest -> Nothing
        Extend -> error "logic"
        (Note n o) -> case nextNote of
          Extend -> Nothing
          _ -> Just $ StopNote (NoteSpec n o inst))
      cur

  mayStartNext = case nextNote of
    (Note n o) -> Just $ StartNote (NoteSpec n o inst) 1
    _ -> Nothing

  len = V.length v

  nextI
    | i < fromIntegral len-1 = i+1
    | otherwise = 0

stopVoice :: Voice -> (Voice, [Music])
stopVoice (Voice _ cur l i) =
    ( Voice 0 Nothing l i
    , maybeToList noteChange)
 where
  noteChange = maybe Nothing (\case
    Rest -> Nothing
    Note n o -> Just $ StopNote $ NoteSpec n o i
    Extend -> error "logic") cur

play :: Music -> IO ()
play (StartNote n@(NoteSpec _ _ i) (MidiVelocity v)) = case i of
  SineSynthAHDSR e ahdsr -> midiNoteOnAHDSR (fromIntegral $ fromEnum e) ahdsr pitch vel
  SineSynth ect -> midiNoteOn (fromIntegral $ unEnvelopeCharacteristicTime ect) pitch vel
  Wind k -> effectOn (fromIntegral k) pitch vel
 where
  (MidiPitch pitch) = noteToMidiPitch n
  vel = CFloat v
play (StopNote n@(NoteSpec _ _ i)) = case i of
  SineSynthAHDSR e ahdsr -> midiNoteOffAHDSR (fromIntegral $ fromEnum e) ahdsr pitch
  SineSynth ect -> midiNoteOff (fromIntegral $ unEnvelopeCharacteristicTime ect) pitch
  Wind _ -> effectOff pitch
 where
  (MidiPitch pitch) = noteToMidiPitch n
