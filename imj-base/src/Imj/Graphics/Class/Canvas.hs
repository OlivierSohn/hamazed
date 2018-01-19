{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Canvas
    ( Canvas(..)
      -- * Reexports
    , Size
    , MonadIO
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)

import           Imj.Geo.Discrete

-- | 'Canvas' describes the ability to query a rendering target characterisitics.
class Canvas e where

  -- | The size of the target (window or terminal). Homogenous to 'Coords' 'Pos',
  -- /not/ to pixels.
  getTargetSize' :: (MonadIO m) => e -> m (Maybe Size)
