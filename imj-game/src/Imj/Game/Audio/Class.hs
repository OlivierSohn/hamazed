{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Audio.Class
  ( Audio(..)
  ) where

import           Control.Monad.IO.Class(MonadIO)
import           Imj.Music

class Audio e where
  triggerLaserSound :: (MonadIO m)
                    => e -> m ()

  playMusic :: (MonadIO m)
            => e -> Music -> Instrument -> m ()
