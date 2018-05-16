{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Audio.Class
  ( Audio(..)
  ) where

import           Imj.Prelude
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Imj.Music.Types

class Audio e where
  -- | The value to use when the user didn'T specify one on the command line.
  defaultAudio
    :: e

  withAudio
    :: (MonadUnliftIO m)
    => e -> m a -> m a

  triggerLaserSound
    :: (MonadIO m)
    => e -> m ()

  playMusic
    :: (MonadIO m)
    => e -> Music -> Instrument -> m ()

-- | Muted audio
instance Audio () where
  defaultAudio = ()
  withAudio _ = id
  triggerLaserSound _ = return ()
  playMusic _ _ _ = return ()
  {-# INLINE defaultAudio #-}
  {-# INLINE playMusic #-}
  {-# INLINE triggerLaserSound #-}
  {-# INLINE withAudio #-}
