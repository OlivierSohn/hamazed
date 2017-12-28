{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Draw.FromMonadReader
       (
       -- ** Draw char(s)
         drawChar
       , drawChars
       -- ** Draw text
       , drawTxt
       , drawStr
       , drawColorStr
       -- ** Draw aligned text
       , drawAlignedTxt_
       , drawAlignedTxt
       , drawAlignedColorStr
       -- ** Render to the physical device
       , renderDrawing
       ) where

import           Imj.Prelude

import           Control.Monad(join)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Data.Text(Text)

import           Imj.Color(LayeredColor(..))
import           Imj.Draw.Class
import           Imj.Geo.Discrete.Types
import           Imj.Text.Alignment
import           Imj.Text.ColorString


-- | Draw a 'ColorString'.
{-# INLINABLE drawColorStr #-}
drawColorStr :: (Draw e, MonadReader e m, MonadIO m)
            => ColorString -> Coords Pos -> m ()
drawColorStr cs pos = do
  d <- asks drawColorStr'
  d cs pos

-- | Draw a 'ColorString' with an 'Alignment' constraint.
{-# INLINABLE drawAlignedColorStr #-}
drawAlignedColorStr :: (Draw e, MonadReader e m, MonadIO m)
                   => Alignment -> ColorString -> m Alignment
drawAlignedColorStr a cs = do
  d <- asks drawAlignedColorStr'
  d a cs

-- | Draws text with 'Alignment'.
{-# INLINABLE drawAlignedTxt_ #-}
drawAlignedTxt_ :: (Draw e, MonadReader e m, MonadIO m)
                => Text
                -> LayeredColor
                -> Alignment
                -> m ()
drawAlignedTxt_ txt colors a = do
  d <- asks drawAlignedTxt_'
  d txt colors a

-- | Draws text with 'Alignment'.
--
-- Returns the 'Alignment' projected on the next line.
{-# INLINABLE drawAlignedTxt #-}
drawAlignedTxt :: (Draw e, MonadReader e m, MonadIO m)
               => Text
               -> LayeredColor
               -> Alignment
               -> m Alignment
drawAlignedTxt txt colors a = do
  d <- asks drawAlignedTxt'
  d txt colors a


{-# INLINABLE drawTxt #-}
drawTxt :: (Draw e, MonadReader e m, MonadIO m)
        => Text
        -> Coords Pos
        -> LayeredColor
        -> m ()
drawTxt txt co la = do
  d <- asks drawTxt'
  d txt co la


{-# INLINABLE drawStr #-}
drawStr :: (Draw e, MonadReader e m, MonadIO m)
        => String
        -> Coords Pos
        -> LayeredColor
        -> m ()
drawStr str co la = do
  d <- asks drawStr'
  d str co la

-- | Draws a 'Char' multiple times, starting at the given coordinates and then
-- moving to the right.
{-# INLINABLE drawChars #-}
drawChars :: (Draw e, MonadReader e m, MonadIO m)
          => Int
          -> Char
          -> Coords Pos
          -> LayeredColor
          -> m ()
drawChars i c co la = do
  d <- asks drawChars'
  d i c co la

{-# INLINABLE drawChar #-}
drawChar :: (Draw e, MonadReader e m, MonadIO m)
         => Char
         -> Coords Pos
         -> LayeredColor
         -> m ()
drawChar c co la = do
  d <- asks drawChar'
  d c co la

-- | Render the drawing to {the screen, the console, etc...}.
{-# INLINABLE renderDrawing #-}
renderDrawing :: (Draw e, MonadReader e m, MonadIO m)
              => m ()
renderDrawing =
  join (asks renderDrawing')
