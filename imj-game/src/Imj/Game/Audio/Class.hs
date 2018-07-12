{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Audio.Class
  ( Audio(..)
  ) where

import           Imj.Prelude
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Imj.Audio.Midi
import           Imj.Music.Types

class Audio e where
  -- | The value to use when the user didn't specify one on the command line.
  defaultAudio
    :: e

  withAudio
    :: (MonadUnliftIO m)
    => e -> MaxMIDIJitter -> m a -> m a

  triggerLaserSound
    :: (MonadIO m)
    => e -> m ()

  playMusic
    :: (MonadIO m)
    => e -> MusicalEvent -> m ()

-- | Muted audio
instance Audio () where
  defaultAudio = ()
  withAudio _ _ = id
  triggerLaserSound _ = return ()
  playMusic _ _ = return ()
  {-# INLINE defaultAudio #-}
  {-# INLINE playMusic #-}
  {-# INLINE triggerLaserSound #-}
  {-# INLINE withAudio #-}
