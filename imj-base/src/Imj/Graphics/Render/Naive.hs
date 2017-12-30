
module Imj.Graphics.Render.Naive
          ( NaiveDraw(..)
          ) where

import Data.Text(unpack)

import Control.Monad.Reader(liftIO)

import System.IO(hFlush, stdout)
import System.Console.ANSI(setCursorPosition, clearFromCursorToScreenEnd)
import System.Console.ANSI.Codes(csi)

import Imj.Geo.Discrete
import Imj.Graphics.Class.Draw
import Imj.Graphics.Class.Render
import Imj.Graphics.Color

{- | FOR TESTS ONLY. For production, please use "Imj.Graphics.Render.Delta".

Naive rendering for the terminal : at every call it sends @color@ and
@position@ change commands, hence
<https://en.wikipedia.org/wiki/Screen_tearing screen tearing> happens very quickly as
a consequence of stdout buffer being automatically flushed to avoid overflow.

To fix this problem, "Imj.Graphics.Render.Delta" uses double buffering techniques
to limit the actual number of rendering commands sent to stdout.
-}
data NaiveDraw = NaiveDraw

move' :: Coords Pos -> IO ()
move' (Coords (Coord y) (Coord x)) =
  setCursorPosition y x

color :: LayeredColor -> IO ()
color (LayeredColor bg fg) = do
  let bgCodes = color8BgSGRToCode bg
      fgCodes = color8FgSGRToCode fg
  putStr $ csi (bgCodes ++ fgCodes) "m"

-- | Direct draw to stdout : don't use for production, this is for tests only
-- and creates heavy screen tearing.
instance Draw NaiveDraw where
    drawChar'      _ b c d   = liftIO $ move' c >> color d >> putChar b
    drawChars'     _ b c d e = liftIO $ move' d >> color e >> putStr (replicate b c)
    drawTxt'       _ b c d   = liftIO $ move' c >> color d >> putStr (unpack b)
    drawStr'       _ b c d   = liftIO $ move' c >> color d >> putStr b
    {-# INLINABLE drawChar' #-}
    {-# INLINABLE drawChars' #-}
    {-# INLINABLE drawTxt' #-}
    {-# INLINABLE drawStr' #-}

-- | Direct draw to stdout : don't use for production, this is for tests only
-- and creates heavy screen tearing.
instance Render NaiveDraw where
    renderToScreen' _         = liftIO $ hFlush stdout
                                        >> setCursorPosition 0 0
                                        >> clearFromCursorToScreenEnd
    {-# INLINABLE renderToScreen' #-}
