{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to draw aligned text. Function names are postfixed with @'@: non-postfixed names are reserved
-- to helper functions in "Draw.Helpers.MonadReader", because using the API through the
-- 'MonadReader' monad is the recommended way.

module Draw.Aligned(
         drawAlignedTxt'
       , drawAlignedTxt_'
       -- * Reexports
       , module Draw.Class
       , Alignment(..)
       ) where

import           Imajuscule.Prelude

import           Data.Text(length)

import           Draw.Class
import           Text.Alignment


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
