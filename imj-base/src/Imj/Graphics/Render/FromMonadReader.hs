{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render.FromMonadReader
       ( usingScissor
       , fill
       , drawChar
       , drawChars
       , drawTxt
       , drawStr
       , drawColorStr
       , drawAlignedTxt_
       , drawAlignedTxt
       , drawAlignedColorStr
       , renderToScreen
       -- * Reexports
       , Scissor, LayeredColor, Coords, Pos, Alignment, ColorString, Draw, Render, MonadReader, MonadIO
       ) where

import           Imj.Prelude

import           Control.Monad(join)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Data.Text(Text)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Color(LayeredColor(..))
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.Text.ColorString


-- | Executes actions in context of a given 'Scissor'.
{-# INLINABLE usingScissor #-}
usingScissor :: (Draw e, MonadReader e m, MonadIO m)
              => Scissor -> m () -> m ()
usingScissor v actions = do
  d <- asks usingScissor'
  d v actions

-- | Fills the region delimited by the 'Scissor' (or the entire screen if no 'Scissor')
-- is in active at the moment.
{-# INLINABLE fill #-}
fill :: (Draw e, MonadReader e m, MonadIO m)
     => Char -> LayeredColor -> m ()
fill char col= do
  d <- asks fill'
  d char col

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
{-# INLINABLE renderToScreen #-}
renderToScreen :: (Render e, MonadReader e m, MonadIO m)
               => m ()
renderToScreen =
  join (asks renderToScreen')
