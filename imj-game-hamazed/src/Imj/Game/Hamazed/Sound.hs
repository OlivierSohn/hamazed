{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Sound
  ( laserSound
  ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay, forkIO)

import           Imj.Audio

laserProgram :: CInt
laserProgram = 11

laserNote :: CShort
laserNote = 60

laserSound :: IO ()
laserSound = do
  effectOn laserProgram laserNote 1
  void $ forkIO $ do
    threadDelay $ 1000*60
    effectOff laserNote
