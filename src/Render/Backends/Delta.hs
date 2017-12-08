{-# LANGUAGE NoImplicitPrelude #-}

module Render.Backends.Delta(
                            beginFrame
                          , endFrame
                          , setForeground
                          , setColors
                          , restoreColors
                          , setRawForeground
                          , restoreForeground
                          , moveTo
                          , renderChar
                          , renderChars
                          , renderStr
                          , renderTxt
                          , preferredBuffering
                          , setRenderSize
                          , getRenderSize
                          , Color8Code(..)
                          ) where

import           Imajuscule.Prelude

import           Control.Monad( void )

import           Data.Text( Text )
import           Data.String( String )

import           System.IO( hFlush
                          , stdout
                          , BufferMode(..) )

import           Geo.Discrete.Types

import           Render.Backends.Internal.Delta

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering Nothing

beginFrame :: IO ()
beginFrame = return ()

endFrame :: IO ()
endFrame = blitBuffer True {- clear buffer -} >> hFlush stdout

moveTo :: Coords -> IO ()
moveTo (Coords (Row r) (Col c))
  | r < 0 || c < 0 = error "cannot render to negative locations"
  | otherwise = bGotoXY (fromIntegral c) (fromIntegral r)

renderChar :: Char -> IO ()
renderChar c = void (bPutCharRaw c)

renderChars :: Int -> Char -> IO ()
renderChars n
  | n < 0 = error "cannot render a negative number of chars"
  | otherwise = bPutChars (fromIntegral n)

renderStr :: String -> IO ()
renderStr = bPutStr

renderTxt :: Text -> IO ()
renderTxt = bPutText

setForeground :: ColorIntensity -> Color -> IO Color8Code
setForeground = bSetForeground

getRenderSize :: IO (Int, Int)
getRenderSize = do
  (w,h) <- bGetRenderSize
  return (fromIntegral w, fromIntegral h)

setRenderSize :: Int -> Int -> IO ()
setRenderSize w h = bSetRenderSize (fromIntegral w) (fromIntegral h)

setRawForeground :: Color8Code -> IO Color8Code
setRawForeground = bSetRawForeground

restoreForeground :: Color8Code -> IO ()
restoreForeground = void . setRawForeground

setColors :: (Color8Code, Color8Code) -> IO (Color8Code, Color8Code)
setColors = bSetColors

restoreColors :: (Color8Code, Color8Code) -> IO ()
restoreColors = void . bSetColors
