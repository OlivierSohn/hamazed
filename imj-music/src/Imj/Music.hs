
module Imj.Music
      ( module Imj.Music.Alter
      , module Imj.Music.Analyze
      , module Imj.Music.Compose
      , module Imj.Music.Instruments
      , module Imj.Music.Play
      , module Imj.Music.PressedKeys
      , module Imj.Music.Record
      , module Imj.Music.Types
      , usingAudio2
      ) where

import Imj.Audio
import Imj.Music.Alter
import Imj.Music.Analyze
import Imj.Music.Compose
import Imj.Music.Instruments
import Imj.Music.Play
import Imj.Music.PressedKeys
import Imj.Music.Record
import Imj.Music.Types


import           Control.Monad.IO.Unlift(MonadUnliftIO)

usingAudio2 :: MonadUnliftIO m => m a -> m a
usingAudio2 = usingAudio
