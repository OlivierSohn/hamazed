
module Color.Types
          ( LayeredColor(..)
          -- * Reexports from System.Console.ANSI
          , Color8Code(..)
          ) where

import           System.Console.ANSI( Color8Code(..) )

data LayeredColor = LayeredColor {
    _colorsBackground :: {-# UNPACK #-} !Color8Code
    -- ^ Background color
  , _colorsForeground :: {-# UNPACK #-} !Color8Code
    -- ^ Foreground color
} deriving(Eq, Show)
