{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.Class.Positionable
            ( Positionable(..)
            ) where

import           Imj.Prelude

import           Control.Monad.Reader.Class(MonadReader, asks)
import qualified Data.Text as Text(length)
import qualified Data.List as List(length)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Graphics.Text.Alignment

-- | A 'Positionable' is a graphical element that knows how to draw itself
-- except for its positions.
class Positionable a where
  -- | Draw at a given position.
  drawAt :: (Draw e, MonadReader e m, MonadIO m)
         => a -> Coords Pos -> m ()

  -- | Return the width of the 'Positionable',
  -- it is used by 'drawAligned'.
  width :: a -> Length Width
  height :: a -> Length Height

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

instance Positionable Text where
  drawAt txt pos = do
    d <- asks drawTxt'
    d txt pos whiteOnBlack
  width = fromIntegral . Text.length
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}

instance Positionable ([] Char) where
  drawAt txt pos = do
    d <- asks drawStr'
    d txt pos whiteOnBlack
  width = fromIntegral . List.length
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}

instance Positionable Char where
  drawAt c pos = do
    d <- asks drawGlyph'
    d (textGlyph c) pos whiteOnBlack
  width _ = 1
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}

instance Positionable Glyph where
  drawAt g pos = do
    d <- asks drawGlyph'
    d g pos whiteOnBlack
  width _ = 1
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}
