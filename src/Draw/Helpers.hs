-- | Helper functions to make the code cleaner at the call site,
--   abstracting away the use of 'ReaderT' function 'asks' to retrieve
--   the 'Draw' functions.

module Draw.Helpers(
       -- * Helper functions
         drawTxt
       , drawChars
       , drawChar
       , flush
       ) where

import           Control.Monad.Reader(ReaderT, asks, join)
import           Data.Text(Text)

import           Color
import           Geo.Discrete.Types
import           Draw.Class

-- | Draw 'Text'
{-# INLINABLE drawTxt #-}
drawTxt :: (Draw e) => Text -> Coords -> LayeredColor -> ReaderT e IO ()
drawTxt txt co la = do
  d <- asks drawTxt_
  d txt co la

-- | Draw a repeated char
{-# INLINABLE drawChars #-}
drawChars :: (Draw e) => Int -> Char -> Coords -> LayeredColor -> ReaderT e IO ()
drawChars i c co la = do
  d <- asks drawChars_
  d i c co la

-- | Draw a 'Char'
{-# INLINABLE drawChar #-}
drawChar :: (Draw e) => Char -> Coords -> LayeredColor -> ReaderT e IO ()
drawChar c co la = do
  d <- asks drawChar_
  d c co la

-- | Flush
{-# INLINABLE flush #-}
flush :: (Draw e) => ReaderT e IO ()
flush =
  join (asks flush_)
