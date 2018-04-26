{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Sound
  ( laserSound
  ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay, forkIO)

import           Imj.Audio

laserSound :: IO ()
laserSound = do
  effectOn 60
  void $ forkIO $ do
    threadDelay $ 1000*60
    effectOff 60
