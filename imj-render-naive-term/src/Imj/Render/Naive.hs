module Imj.Render.Naive
          ( NaiveDraw(..)
          ) where

import Data.Text(unpack)

import Control.Monad.Reader(liftIO)

import System.IO(hFlush, stdout)
import System.Console.ANSI(setCursorPosition)
import System.Console.ANSI.Codes(csi)

import Imj.Draw.Class

-- | This would make a good candidate for (very) naive rendering benchmark.
--
-- It is very naïve in the sense that it doesn't have any state, so
-- at every call, it sets the color and the position, even if it's not needed.
--
-- It is also naïve in the sense that there is no double buffering involved.
--
-- We could do a smarter version with IORef state to benchmark.
newtype NaiveDraw = NaiveDraw Int -- Int is needed, () doesn't compile

move :: Coords -> IO ()
move (Coords (Coord y) (Coord x)) =
  setCursorPosition y x

color :: LayeredColor -> IO ()
color (LayeredColor bg fg) = do
  let bgCodes = color8BgSGRToCode bg
      fgCodes = color8FgSGRToCode fg
  putStr $ csi (bgCodes ++ fgCodes) "m"

instance Draw NaiveDraw where
    drawChar'      _ b c d   = liftIO $ move c >> color d >> putChar b
    drawChars'     _ b c d e = liftIO $ move d >> color e >> putStr (replicate b c)
    drawTxt'       _ b c d   = liftIO $ move c >> color d >> putStr (unpack b)
    drawStr'       _ b c d   = liftIO $ move c >> color d >> putStr b
    renderDrawing' _         = liftIO $ hFlush stdout
    {-# INLINABLE drawChar' #-}
    {-# INLINABLE drawChars' #-}
    {-# INLINABLE drawTxt' #-}
    {-# INLINABLE drawStr' #-}
    {-# INLINABLE renderDrawing' #-}
