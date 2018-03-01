{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render.FromMonadReader
       ( usingScissor
       , fill
       , drawGlyph
       , drawGlyphs
       , drawTxt
       , drawStr
       , drawColorStr
       , drawMultiLineStr
       , cycleRenderingOptions
       , getTargetSize
       , renderToScreen
       -- * Reexports
       , Scissor, LayeredColor, Coords, Pos, Alignment, ColorString
       , Draw, Render, Canvas, MonadReader, MonadIO
       ) where

import           Imj.Prelude
import qualified Prelude(length)

import           Control.Monad(join)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Data.Text(Text)

import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Canvas
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Color(LayeredColor(..))
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.Text.ColorString


-- | Executes actions in context of a given 'Scissor'.
{-# INLINABLE usingScissor #-}
usingScissor :: (Draw e, MonadReader e m, MonadIO m)
              => Scissor -> m a -> m a
usingScissor v actions = do
  d <- asks usingScissor'
  d v actions

-- | Fills the region delimited by the 'Scissor' (or the entire screen if no 'Scissor')
-- is in active at the moment.
{-# INLINABLE fill #-}
fill :: (Draw e, MonadReader e m, MonadIO m)
     => Glyph -> LayeredColor -> m ()
fill g col= do
  d <- asks fill'
  d g col

-- | Draw a 'ColorString'.
{-# INLINABLE drawColorStr #-}
drawColorStr :: (Draw e, MonadReader e m, MonadIO m)
             => ColorString -> Coords Pos -> m ()
drawColorStr cs pos = do
  d <- asks drawColorStr'
  d cs pos

-- | Draw a 'String' on multiple lines.
drawMultiLineStr :: (Render e, MonadReader e m, MonadIO m)
                 => String
                 -> Coords Pos
                 -- ^ The text will be centered vertically and left justified
                 -- according to this position.
                 -> LayeredColor
                 -> Int
                 -- ^ Maximum line size.
                 -> m ()
drawMultiLineStr str ref' color nChars = do
  let strs = multiLine nChars str
      -- center vertically
      ref = move (quot (Prelude.length strs) 2) Up ref'
  zipWithM_
    (\i s -> drawStr s (move i Down ref) color)
    [0..] strs


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
{-# INLINABLE drawGlyphs #-}
drawGlyphs :: (Draw e, MonadReader e m, MonadIO m)
          => Int
          -> Glyph
          -> Coords Pos
          -> LayeredColor
          -> m ()
drawGlyphs i g co la = do
  d <- asks drawGlyphs'
  d i g co la

{-# INLINABLE drawGlyph #-}
drawGlyph :: (Draw e, MonadReader e m, MonadIO m)
         => Glyph
         -> Coords Pos
         -> LayeredColor
         -> m ()
drawGlyph g co la = do
  d <- asks drawGlyph'
  d g co la


{-# INLINABLE cycleRenderingOptions #-}
cycleRenderingOptions :: (Render e, MonadReader e m, MonadIO m) => m ()
cycleRenderingOptions =
  join $ asks cycleRenderingOptions'


{-# INLINABLE getTargetSize #-}
getTargetSize :: (Canvas e, MonadReader e m, MonadIO m)
              => m (Maybe Size)
getTargetSize =
  join (asks getTargetSize')

-- | Render the drawing.
{-# INLINABLE renderToScreen #-}
renderToScreen :: (Render e, MonadReader e m, MonadIO m)
               => m (Time Duration System, Time Duration System, Time Duration System)
renderToScreen =
  join (asks renderToScreen')
