{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-| The step*** functions return a list of 'MusicalEvent' which in turn can be
played using 'play'.
-}

module Imj.Music.Play
      ( -- * Step a Score
        stepScore
      , stopScore
      -- * Step a Voice
      , stepVoice
      , stopVoice
      , stepNVoice
      , stepNVoiceAndStop
      -- * Play a MusicalEvent
      , play
      -- * Utilities
      , playAtTempo
      , playVoicesAtTempo
      , allMusic
      ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay)
import           Data.List(foldl')
import           Data.Maybe(catMaybes, maybeToList)
import qualified Data.Vector as V
import           Foreign.C

import           Imj.Audio
import           Imj.Music.Types

playVoicesAtTempo :: Float
            -- ^ Beats per minute
            -> Instrument
            -> [[VoiceInstruction]]
            -> IO ()
playVoicesAtTempo tempo i =
  playMusic tempo . foldl' (zipWith (++)) (repeat []) . map (allMusic i)

-- | Plays a series of 'VoiceInstruction' at a constant tempo, using an 'Instrument'.
playAtTempo :: Float
            -- ^ Beats per minute
            -> Instrument
            -> [VoiceInstruction]
            -> IO ()
playAtTempo tempo i =
  playMusic tempo . allMusic i

playMusic :: Float
            -- ^ Beats per minute
            -> [[MusicalEvent]]
            -> IO ()
playMusic tempo m =
  go m
 where
  go [] = return()
  go (n:ns) = do
    mapM_ play n
    threadDelay pause
    go ns

  pause = round $ 1000*1000*60/tempo

-- TODO use ids for notes (one id per voice would be enough), to support this case well,
-- where voice1 and voice2 are assigned to the same instrument:
-- voice1: do - - - -
-- voice2: . . do . .
-- when the do of voice2 terminates, we want it to fadeout its own channel, not the one
-- of the other voice. NOTE it makes a difference only if the 2 notes have different velocities,
-- which is not possible as of today.
-- | Steps a 'Score' forward (by a single time quantum),
-- returns the list of 'MusicalEvent's that need to be played for this time quantum.
stepScore :: Score
          -> (Score, [MusicalEvent])
stepScore (Score l) = (s,m)
 where
  nv = map stepVoice l
  s = Score $ map fst nv
  m = concatMap snd nv

-- | Returns the 'MusicalEvent's that need to be played to stop the ongoing notes
-- associated to this 'Score'.
stopScore :: Score
          -> (Score, [MusicalEvent])
stopScore (Score l) = (s,m)
 where
  nv = map stopVoice l
  s = Score $ map fst nv
  m = concatMap snd nv

-- | Function used mainly for testing purposes.
allMusic :: Instrument -> [VoiceInstruction] -> [[MusicalEvent]]
allMusic i x =
  snd $ stepNVoiceAndStop (sizeVoice s) s
 where
  s = mkVoice i x

sizeVoice :: Voice -> Int
sizeVoice (Voice _ _ v _) = V.length v

-- | Like 'stepNVoice' but also uses 'stopVoice' to finalize the music.
stepNVoiceAndStop :: Int -> Voice -> (Voice, [[MusicalEvent]])
stepNVoiceAndStop n s =
  (s'', reverse $ lastMusic:music)
 where
  (s', music) = stepNVoiceReversed n s
  (s'', lastMusic) = stopVoice s'

stepNVoiceReversed :: Int -> Voice -> (Voice, [[MusicalEvent]])
stepNVoiceReversed n score
  | n < 0 = (score,[])
  | otherwise = go n score []
 where
  go 0 s l = (s, l)
  go i s l = let (s',m) = stepVoice s in go (i-1) s' $ m:l

stepNVoice :: Int -> Voice -> (Voice, [[MusicalEvent]])
stepNVoice n score = let (s,l) = stepNVoiceReversed n score in (s,reverse l)

-- | Steps a 'Voice' forward (by a single time quantum),
-- returns the list of 'MusicalEvent's that need to be played for this time quantum.
stepVoice :: Voice
          -> (Voice, [MusicalEvent])
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
          _ -> Just $ StopNote (InstrumentNote n o inst))
      cur

  mayStartNext = case nextNote of
    (Note n o) -> Just $ StartNote (InstrumentNote n o inst) 1
    _ -> Nothing

  len = V.length v

  nextI
    | i < fromIntegral len-1 = i+1
    | otherwise = 0

-- | Returns the 'MusicalEvent's that need to be played to stop the ongoing notes
-- associated to this 'Voice'.
stopVoice :: Voice -> (Voice, [MusicalEvent])
stopVoice (Voice _ cur l i) =
    ( Voice 0 Nothing l i
    , maybeToList noteChange)
 where
  noteChange = maybe Nothing (\case
    Rest -> Nothing
    Note n o -> Just $ StopNote $ InstrumentNote n o i
    Extend -> error "logic") cur

-- | Plays a 'MusicalEvent'
play :: MusicalEvent -> IO ()
play (StartNote n@(InstrumentNote _ _ i) (NoteVelocity v)) = case i of
  SineSynthAHDSR e ahdsr -> midiNoteOnAHDSR (fromIntegral $ fromEnum e) ahdsr pitch vel
  SineSynth ect -> midiNoteOn (fromIntegral $ unEnvelopeCharacteristicTime ect) pitch vel
  Wind k -> effectOn (fromIntegral k) pitch vel
 where
  (MidiPitch pitch) = instrumentNoteToMidiPitch n
  vel = CFloat v
play (StopNote n@(InstrumentNote _ _ i)) = case i of
  SineSynthAHDSR e ahdsr -> midiNoteOffAHDSR (fromIntegral $ fromEnum e) ahdsr pitch
  SineSynth ect -> midiNoteOff (fromIntegral $ unEnvelopeCharacteristicTime ect) pitch
  Wind _ -> effectOff pitch
 where
  (MidiPitch pitch) = instrumentNoteToMidiPitch n
