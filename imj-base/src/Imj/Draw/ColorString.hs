{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to draw 'ColorString'.

module Imj.Draw.ColorString
          ( drawColored
          , drawColoredAligned
          ) where

import           Imj.Prelude

import           Control.Monad(foldM_)
import           Control.Monad.Reader(MonadReader)
import           Data.Text(length)

import           Imj.Draw.Class
import           Imj.Draw.Helpers.MonadReader
import           Imj.Geo.Discrete
import           Imj.Text.Alignment
import           Imj.Text.ColorString

-- | Draw a 'ColorString'.
{-# INLINABLE drawColored #-}
drawColored :: (Draw e, MonadReader e m, MonadIO m)
            => ColorString
            -> Coords
            -> m ()
drawColored (ColorString cs) pos =
  foldM_
    (\count (txt, color) -> do
      let l = length txt
      drawTxt txt (move count RIGHT pos) color
      return $ count + l
    ) 0 cs

-- | Draw a 'ColorString' with an 'Alignment' constraint.
{-# INLINABLE drawColoredAligned #-}
drawColoredAligned :: (Draw e, MonadReader e m, MonadIO m)
            => Alignment
            -> ColorString
            -> m Alignment
drawColoredAligned a cs = do
  let leftCorner = align' a (countChars cs)
  _ <- drawColored cs leftCorner
  return $ toNextLine a
