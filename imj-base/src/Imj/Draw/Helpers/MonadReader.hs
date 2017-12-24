{-# LANGUAGE NoImplicitPrelude #-}

-- | The functions of this module contain the boilerplate code to call the
-- 'Draw' functions from a 'MonadReader' monad.

module Imj.Draw.Helpers.MonadReader
       (
       -- * Draw char(s)
         drawChar
       , drawChars
       -- * Draw text
       , drawTxt
       , drawStr
       -- * Draw aligned text
       , drawAlignedTxt_
       , drawAlignedTxt
       -- * Render to the physical device
       , renderDrawing
       -- * Reexports
       , module Imj.Geo.Discrete.Types
       , module Imj.Draw.Class
       , MonadReader -- to have the link to it in Haddock
       ) where

import           Imj.Prelude

import           Control.Monad(join)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Data.Text(Text)

import           Imj.Draw.Aligned
import           Imj.Draw.Class
import           Imj.Geo.Discrete.Types

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


{-# INLINABLE drawStr #-}
drawStr :: (Draw e, MonadReader e m, MonadIO m)
        => String
        -> Coords
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

-- | Render the drawing to {the screen, the console, etc...}.
{-# INLINABLE renderDrawing #-}
renderDrawing :: (Draw e, MonadReader e m, MonadIO m)
              => m ()
renderDrawing =
  join (asks renderDrawing')
