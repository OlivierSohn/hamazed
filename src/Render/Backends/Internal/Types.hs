-- | Types used for Delta rendering.

module Render.Backends.Internal.Types
          ( Colors(..)
          -- * Reexports from System.Console.ANSI
          , Color8Code(..)
          ) where


import           System.Console.ANSI( Color8Code(..) )

data Colors = Colors {
    _colorsBackground :: {-# UNPACK #-} !Color8Code
    -- ^ Background color
  , _colorsForeground :: {-# UNPACK #-} !Color8Code
    -- ^ Foreground color
}
