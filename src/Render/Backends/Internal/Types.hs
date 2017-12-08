module Render.Backends.Internal.Types
          ( Colors(..)
          ) where


import           System.Console.ANSI( Color8Code(..) )

data Colors = Colors {
    _colorsForeground :: {-# UNPACK #-} !Color8Code
  , _colorsBackground :: {-# UNPACK #-} !Color8Code
}
