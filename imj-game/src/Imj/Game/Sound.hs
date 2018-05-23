{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Sound
  ( laserSound
  ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay, forkIO)

import           Imj.Music.Types
import           Imj.Music.Play

laserProgram :: Int
laserProgram = 11

laserNote :: MidiPitch
laserNote = 60

laserSound :: IO ()
laserSound = do
  let (noteName,octave) = midiPitchToNoteAndOctave laserNote
      n = NoteSpec noteName octave (Wind laserProgram)
  play $ StartNote n $ MidiVelocity 1
  void $ forkIO $ do
    threadDelay $ 1000*60
    play $ StopNote n
