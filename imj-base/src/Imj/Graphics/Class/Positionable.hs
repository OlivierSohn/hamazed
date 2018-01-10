{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Positionable
            ( Positionable(..)
            ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Render.FromMonadReader

-- | A 'Positionable' is a graphical element that knows how to draw itself
-- except for its positions.
class Positionable a where
  -- | Draw at a given position.
  drawAt :: (Draw e, MonadReader e m, MonadIO m)
         => a -> Coords Pos -> m ()

  -- | Return the width of the 'Positionable',
  -- it is used by 'drawAligned'.
  width :: a -> Length Width

  {-# INLINABLE drawAligned #-}
  -- | Draw 'Positionable' aligned w.r.t alignment and reference coordinates.
  --
  -- Return an 'Alignment' where the reference coordinate of the input 'Alignment'
  -- was projected on the next line.
  drawAligned :: (Draw e, MonadReader e m, MonadIO m)
              => a -> Alignment -> m Alignment
  drawAligned p alignment = do
    let w = width p
        pos = align' alignment w
    drawAt p pos
    return $ toNextLine alignment

  {-# INLINABLE drawAligned_ #-}
  -- | Draw 'Positionable' aligned w.r.t alignment and reference coordinates.
  drawAligned_ :: (Draw e, MonadReader e m, MonadIO m)
               => a -> Alignment -> m ()
  drawAligned_ p al =
    void $ drawAligned p al

-- Due to a cyclic dependency, this instance cannot be in ColorString module.
instance Positionable ColorString where
  drawAt = drawColorStr
  {-# INLINABLE drawAt #-}
  width = fromIntegral . countChars
  {-# INLINABLE width #-}
