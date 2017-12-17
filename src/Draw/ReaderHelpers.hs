-- | Helper functions to make the code cleaner at the call site.

module Draw.ReaderHelpers(
       -- * Helper functions
         drawTxt
       , drawAlignedTxt_
       , drawAlignedTxt
       , drawChars
       , drawChar
       , renderDrawing
       -- * Reexports
       , module Color
       ) where

import           Control.Monad(join)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Data.Text(Text)

import           Color
import           Geo.Discrete.Types
import           Draw.Class



-- | Draws text aligned w.r.t alignment and reference coodinates.
{-# INLINABLE drawAlignedTxt_ #-}
drawAlignedTxt_ :: (Draw e, MonadReader e m, MonadIO m)
                => Text
                -> LayeredColor
                -> Alignment
                -> m ()
drawAlignedTxt_ txt colors a = do
  d <- asks drawAlignedTxt''
  d txt colors a

-- | Draws text aligned w.r.t alignment and reference coodinates.
--
-- Returns the alignment projected on the next line.
{-# INLINABLE drawAlignedTxt #-}
drawAlignedTxt :: (Draw e, MonadReader e m, MonadIO m)
               => Text
               -> LayeredColor
               -> Alignment
               -> m Alignment
drawAlignedTxt txt colors a = do
  d <- asks drawAlignedTxt'
  d txt colors a


-- | Draw 'Text'
{-# INLINABLE drawTxt #-}
drawTxt :: (Draw e, MonadReader e m, MonadIO m)
        => Text
        -> Coords
        -> LayeredColor
        -> m ()
drawTxt txt co la = do
  d <- asks drawTxt_
  d txt co la

-- | Draw a repeated char
{-# INLINABLE drawChars #-}
drawChars :: (Draw e, MonadReader e m, MonadIO m)
          => Int
          -> Char
          -> Coords
          -> LayeredColor
          -> m ()
drawChars i c co la = do
  d <- asks drawChars_
  d i c co la

-- | Draw a 'Char'
{-# INLINABLE drawChar #-}
drawChar :: (Draw e, MonadReader e m, MonadIO m)
         => Char
         -> Coords
         -> LayeredColor
         -> m ()
drawChar c co la = do
  d <- asks drawChar_
  d c co la

-- | Render what was drawn
{-# INLINABLE renderDrawing #-}
renderDrawing :: (Draw e, MonadReader e m, MonadIO m)
              => m ()
renderDrawing =
  join (asks renderDrawing_)
