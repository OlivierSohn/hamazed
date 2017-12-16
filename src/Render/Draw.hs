{- | This modules provides a way to abstract a global renderer, in a style adhering to
<https://www.fpcomplete.com/blog/2017/06/readert-design-pattern these recommendations>
regarding global state in a program.

The idea is the following: you embed the drawing functions in your "environment" 'Env'
by writing an instance of 'Draw' for 'Env'.

Then, using a 'ReaderT' 'Env'
monad transformer, and the helper functions defined in this module you can write:

@
helloWorld :: (Draw e) => ReaderT e IO ()
helloWorld = do
  drawTxt "Hello" (Coords 10 10) red
  drawTxt "World" (Coords 20 20) green
  flush

main = do
  env <- createEnv
  runReaderT helloWorld env
@

In its file 'src/Env.hs',
<https://github.com/OlivierSohn/hamazed this game>
shows an example implementation
of 'Env', 'createEnv', and the 'Draw' instance of 'Env',
-}


module Render.Draw(
         Draw(..)
       -- * Helper functions
       , drawTxt
       , drawChars
       , drawChar
       , flush
       -- * reexports
       , ReaderT
       ) where

import           Control.Monad.Reader(ReaderT, asks, join)
import           Data.Text(Text)

import           Color.Types
import           Geo.Discrete.Types

-- | Instances of this class can render colored text in the console,
-- and will be argument of a ReaderT.
class Draw e where
  drawChar_ :: e -> (Char -> Coords -> LayeredColor -> ReaderT e IO ())
  drawChar_ = undefined

  drawChars_ :: e -> (Int -> Char -> Coords -> LayeredColor -> ReaderT e IO ())
  drawChars_ = undefined

  drawTxt_ :: e -> (Text -> Coords -> LayeredColor -> ReaderT e IO ())
  drawTxt_ = undefined

  flush_ :: e -> ReaderT e IO ()
  flush_ = undefined

--------------------------------------------------------------------------------
-- Helper functions to make the code cleaner at the call site.
--------------------------------------------------------------------------------

-- | Draw text
{-# INLINABLE drawTxt #-}
drawTxt :: (Draw e) => Text -> Coords -> LayeredColor -> ReaderT e IO ()
drawTxt txt co la = do
  d <- asks drawTxt_
  d txt co la

-- | Draw chars
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
