-- | The functions of this module contain the boilerplate code to call the
-- 'Draw' functions from a 'MonadReader' monad.

module Draw.Helpers.MonadReader
       (
       -- * Draw char(s)
         drawChar
       , drawChars
       -- * Draw text
       , drawTxt
       -- * Draw aligned text
       , drawAlignedTxt_
       , drawAlignedTxt
       -- * Render to the physical device
       , renderDrawing
       -- * Reexports
       , module Color
       , module Geo.Discrete.Types
       , module Draw.Class
       , MonadReader -- to have the link to it in Haddock
       ) where

import           Control.Monad(join)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Data.Text(Text)

import           Color
import           Geo.Discrete.Types
import           Draw.Class

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
        -> Coords
        -> LayeredColor
        -> m ()
drawTxt txt co la = do
  d <- asks drawTxt'
  d txt co la

-- | Draws a 'Char' multiple times, starting at the given coordinates and then
-- moving to the right.
{-# INLINABLE drawChars #-}
drawChars :: (Draw e, MonadReader e m, MonadIO m)
          => Int
          -> Char
          -> Coords
          -> LayeredColor
          -> m ()
drawChars i c co la = do
  d <- asks drawChars'
  d i c co la

{-# INLINABLE drawChar #-}
drawChar :: (Draw e, MonadReader e m, MonadIO m)
         => Char
         -> Coords
         -> LayeredColor
         -> m ()
drawChar c co la = do
  d <- asks drawChar'
  d c co la

-- | Render the drawing to the physical device (the screen, the console etc...).
{-# INLINABLE renderDrawing #-}
renderDrawing :: (Draw e, MonadReader e m, MonadIO m)
              => m ()
renderDrawing =
  join (asks renderDrawing')
