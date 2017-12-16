-- | This module handles aligned text.

module Render.Aligned
            ( Alignment(..)
            , drawAlignedTxt
            , drawAlignedTxt_
            , align
            , align'
            ) where

import           Prelude hiding (length)

import           Data.Text(Text, length)

import           Geo.Discrete
import           Render.Draw
import           Render.Helpers

-- | Specifies where the 'Text' should be drawn w.r.t the reference coordinates.
data Alignment = Centered
               -- ^ Draw the text centered on reference coordinates.
               | RightAligned
               -- ^ Draw the text left of the reference coordinates.


-- | Draws text aligned w.r.t alignment and reference coodinates.
{-# INLINABLE drawAlignedTxt_ #-}
drawAlignedTxt_ :: (Draw e)
                  => Alignment
                  -> Text
                  -> Coords
                  -- ^ Reference coordinates
                  -> LayeredColor
                  -> ReaderT e IO ()
drawAlignedTxt_ a txt pos colors = do
  let leftCorner = align' a (length txt) pos
  drawTxt txt leftCorner colors

-- | Draws text aligned w.r.t alignment and reference coodinates.
--
-- Returns the reference coordinate projected on the next line.
{-# INLINABLE drawAlignedTxt #-}
drawAlignedTxt :: (Draw e)
                 => Alignment
                 -> Text
                 -> Coords
                 -- ^ Reference coordinates
                 -> LayeredColor
                 -> ReaderT e IO Coords
drawAlignedTxt a txt pos colors =
  drawAlignedTxt_ a txt pos colors >> return (translateInDir Down pos)


-- | Computes starting coordinates where from we should draw a series of characters
--  of a given length, to meet the alignment constraint w.r.t the reference coordinates.
align' :: Alignment
       -> Int
       -- ^ number of characters to draw
       -> Coords
       -- ^ Reference coordinates
       -> Coords
align' a count ref =
  let (amount, dir) = align a count
  in move amount dir ref

-- | Given a number of characters and an alignment, returns the displacement
-- that should be done relatively to the reference coordinates in order to find
-- the first character position.
align :: Alignment
      -> Int
      -- ^ Count of characters
      -> (Int, Direction)
align a count =
  (amount, LEFT)
 where
  amount =
    case a of
      Centered     -> 1 + quot count 2
      RightAligned -> count
