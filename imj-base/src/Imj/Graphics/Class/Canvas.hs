{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Canvas
    ( Canvas(..)
      -- * Reexports
    , Size
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types

-- | 'Canvas' describes a rendering target.
class Canvas e where

  -- | Returns the size of the target (window or terminal), in number of /cells/
  -- (homogenous to 'Coords' 'Pos', /not/ to pixels, also see 'PPU').
  getTargetSize' :: (MonadIO m) => e -> m (Maybe Size)

  -- | Discard all cached data, including any object whose size is based on a previous
  -- window size.
  --
  -- In practice, it will be called to inform of a change in the number of pixels
  -- of the target (in either dimension), so that the 'Canvas' reconfigures itself
  -- accordingly: eventhough not all changes in number of pixels result in a change in
  -- target size, discarding cached data is necessary so as to avoid rendering artifats
  -- due to the change in number of pixels.
  onTargetChanged' :: (MonadIO m) => e -> m (Either String ())
