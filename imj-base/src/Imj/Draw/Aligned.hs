{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Draw.Aligned(
         drawAlignedTxt'
       , drawAlignedTxt_'
       ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Data.Text(length)

import           Imj.Color(LayeredColor(..))
import           Imj.Draw.Class
import           Imj.Text.Alignment


-- | Draws text aligned w.r.t alignment and reference coodinates.
{-# INLINABLE drawAlignedTxt_' #-}
drawAlignedTxt_' :: (Draw e, MonadIO m)
                 => e
                 -> Text
                 -> LayeredColor
                 -> Alignment
                 -> m ()
drawAlignedTxt_' env txt colors a = do
  let leftCorner = align' a (length txt)
  drawTxt' env txt leftCorner colors

-- | Draws text aligned w.r.t alignment and reference coodinates.
--
-- Returns an 'Alignment' where the reference coordinate of the input 'Alignment'
-- was projected on the next line.
{-# INLINABLE drawAlignedTxt' #-}
drawAlignedTxt' :: (Draw e, MonadIO m)
                => e
                -> Text
                -> LayeredColor
                -> Alignment
                -> m Alignment
drawAlignedTxt' env txt colors a =
  drawAlignedTxt_' env txt colors a
    >> return (toNextLine a)
