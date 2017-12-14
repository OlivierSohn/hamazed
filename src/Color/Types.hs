{-# OPTIONS_HADDOCK hide #-}

module Color.Types
          ( LayeredColor(..)
          -- * Reexports from System.Console.ANSI
          , Color8Code(..)
          ) where

import           System.Console.ANSI( Color8Code(..) )

-- | A background and a foreground <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 8-bit color>.
data LayeredColor = LayeredColor {
    _colorsBackground :: {-# UNPACK #-} !Color8Code
    -- ^ Background color
  , _colorsForeground :: {-# UNPACK #-} !Color8Code
    -- ^ Foreground color
} deriving(Eq, Show)
