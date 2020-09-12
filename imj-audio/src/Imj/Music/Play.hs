{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
When the note-on, note-off events come from the user, in real-time, use the 'play' function.

When the music events are known in advance, but you want ot control the pace at which
the music will be executed, use the notion of 'Score' with 'stepScore', 'stopScore'
or the notion of 'Voice' with 'stepVoice', 'stopVoice'.

When the music events are known in advance, and you want to play them at a fixed tempo,
use 'playAtTempo', 'playVoicesAtTempo'
-}

module Imj.Music.Play
      (
      -- * Types
        MusicalEvent
      , PlayResult
      -- * Conversion
      , allMusic
      -- * Play Instruction(s) all at once, with known tempo
      , playAtTempo
      , playVoicesAtTempo
      , playScoreAtTempo
      -- * Create MusicalEvent(s) for a time quantum
      -- ** From a Voice
      , stepVoice
      , stopVoice
      , stepNVoice
      , stepNVoiceAndStop
      -- ** From a Score
      , stepScore
      , stopScore
      ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay)
import           Data.Maybe(catMaybes, maybeToList)
import qualified Data.Vector as V

import           Imj.Audio.Output
import           Imj.Music.Score
import           Imj.Music.Instruction
import           Imj.Music.Instrument
import           Imj.Timing

-- | Plays a series of 'Instruction' at a constant tempo, using an 'Instrument'.
playAtTempo :: Double
            -- ^ Beats per minute
            -> Instrument
            -> [Instruction]
            -> IO PlayResult
playAtTempo tempo i instructions =
  playVoicesAtTempo tempo i [instructions]


playVoicesAtTempo :: Double
                  -- ^ Beats per minute
                  -> Instrument
                  -> [[Instruction]]
                  -> IO PlayResult
playVoicesAtTempo tempo i instructions =
  playScoreAtTempo 1 tempo (mkScore i instructions)

playScoreAtTempo :: Int -> Double -> Score Instrument -> IO PlayResult
playScoreAtTempo count_repetitions tempo s =
  getSystemTime >>=
    go count_repetitions (scoreLength s) (scoreLength s) s 0
 where
  go a b c d e firstTime =
    go' a b c d e
   where
    go' repetitions n 0 _ total
      | repetitions <= 1 = return $ Right ()
      | otherwise = go' (pred repetitions) n n s total
    go' repetitions nn n score total = do
      now <- getSystemTime
      let (newScore, instructions) = stepScore score
          newTimeApprox = addDuration (fromSecs (total * pause)) firstTime
          duration = fromIntegral $ toMicros $ now...newTimeApprox
      when (duration > 0) $ threadDelay duration
      r <- mapM play instructions
      if null $ lefts r
        then
          go' repetitions nn (pred n) newScore (succ total)
        else
          return $ Left ()

    pause = 60/tempo

type PlayResult = Either () ()

allMusic :: i -> [Instruction] -> [[MusicalEvent i]]
allMusic i x =
  snd $ stepNVoiceAndStop (sizeVoice s) s
 where
  s = mkVoice i x


-- TODO use ids for notes (one id per voice would be enough), to support this case well,
-- where voice1 and voice2 are assigned to the same instrument:
-- voice1: do - - - -
-- voice2: . . do . .
-- when the do of voice2 terminates, we want it to fadeout its own channel, not the one
-- of the other voice. NOTE it makes a difference only if the 2 notes have different velocities,
-- which is not possible as of today.
-- | Steps a 'Score' forward (by a single time quantum),
-- returns the list of 'MusicalEvent's that need to be played for this time quantum.
stepScore :: Score i
          -> (Score i, [MusicalEvent i])
stepScore (Score l) = (s,m)
 where
  nv = map stepVoice l
  s = Score $ map fst nv
  m = concatMap snd nv

-- | Returns the 'MusicalEvent's that need to be played to stop the ongoing notes
-- associated to this 'Score'.
stopScore :: Score i
          -> (Score i, [MusicalEvent i])
stopScore (Score l) = (s,m)
 where
  nv = map stopVoice l
  s = Score $ map fst nv
  m = concatMap snd nv

sizeVoice :: Voice i -> Int
sizeVoice (Voice _ _ v _) = V.length v

-- | Steps a 'Voice' forward by a single time quantum,
-- returns the list of 'MusicalEvent's that need to be played for this time quantum.
stepVoice :: Voice i
          -> (Voice i, [MusicalEvent i])
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
          _ -> Just $ StopNote Nothing (InstrumentNote n o inst))
      cur

  mayStartNext = case nextNote of
    (Note n o) -> Just $ StartNote Nothing (InstrumentNote n o inst) 1
    _ -> Nothing

  len = V.length v

  nextI
    | i < fromIntegral len-1 = i+1
    | otherwise = 0

-- | Returns the 'MusicalEvent's that need to be played to stop the ongoing notes
-- associated to this 'Voice'.
stopVoice :: Voice i -> (Voice i, [MusicalEvent i])
stopVoice (Voice _ cur l i) =
    ( Voice 0 Nothing l i
    , maybeToList noteChange)
 where
  noteChange = maybe Nothing (\case
    Rest -> Nothing
    Note n o -> Just $ StopNote Nothing $ InstrumentNote n o i
    Extend -> error "logic") cur

-- | Like 'stepNVoice' but also uses 'stopVoice' to finalize the music.
stepNVoiceAndStop :: Int -> Voice i -> (Voice i, [[MusicalEvent i]])
stepNVoiceAndStop n s =
  (s'', reverse $ lastMusic:music)
 where
  (s', music) = stepNVoiceReversed n s
  (s'', lastMusic) = stopVoice s'

stepNVoiceReversed :: Int -> Voice i -> (Voice i, [[MusicalEvent i]])
stepNVoiceReversed n score
  | n < 0 = (score,[])
  | otherwise = go n score []
 where
  go 0 s l = (s, l)
  go i s l = let (s',m) = stepVoice s in go (i-1) s' $ m:l

-- | Steps a 'Voice' forward by several time quantums
stepNVoice :: Int
           -- ^ How many time quantums to step.
           -> Voice i
           -> (Voice i, [[MusicalEvent i]])
stepNVoice n score = let (s,l) = stepNVoiceReversed n score in (s,reverse l)
