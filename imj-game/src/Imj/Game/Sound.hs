{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Sound
  ( laserSound
  ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay, forkIO)

import           Imj.Audio

laserNote :: MidiPitch
laserNote = 60

laserSound :: NoteVelocity -> IO ()
laserSound velocity = do
  let (noteName,octave) = midiPitchToNoteAndOctave laserNote
      envelope = AHDSR'Envelope
          100 1200 200 15600
          (Eased EaseIn Exp)
          (Eased EaseInOut Exp)
          (Eased EaseOut Exp)
          0.665
      n = InstrumentNote noteName octave (Synth Noise AutoRelease envelope)
      voiceId = VoiceId 0
  play (StartNote Nothing n velocity panCentered) voiceId >>= either
    (return)
    (const $ void $ forkIO $ do
      threadDelay (1000*600)  -- microseconds
      void $ play (StopNote Nothing n) voiceId
      return ())
