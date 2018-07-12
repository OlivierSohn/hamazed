{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Sound
  ( laserSound
  ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay, forkIO)

import           Imj.Audio

laserProgram :: Int
laserProgram = 11

laserNote :: MidiPitch
laserNote = 60

laserSound :: IO ()
laserSound = do
  let (noteName,octave) = midiPitchToNoteAndOctave laserNote
      n = InstrumentNote noteName octave (Wind laserProgram)
  play (StartNote Nothing n $ NoteVelocity 1) >>= either
    (return)
    (const $ void $ forkIO $ do
      threadDelay (1000*60)
      void $ play (StopNote Nothing n)
      return ())
