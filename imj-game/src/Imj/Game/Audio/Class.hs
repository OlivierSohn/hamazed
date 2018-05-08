{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Audio.Class
  ( Audio(..)
  ) where

import           Imj.Prelude
import           Control.Monad.IO.Class(MonadIO)
import           Imj.Music

class Audio e where
  triggerLaserSound :: (MonadIO m)
                    => e -> m ()

  playMusic :: (MonadIO m)
            => e -> Music -> Instrument -> m ()

-- | Muted audio
instance Audio () where
  triggerLaserSound _ = return ()
  playMusic _ _ _ = return ()
