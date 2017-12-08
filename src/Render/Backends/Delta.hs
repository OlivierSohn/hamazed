{-# LANGUAGE NoImplicitPrelude #-}

module Render.Backends.Delta(
                            beginFrame
                          , endFrame
                          , restoreColors
                          , moveTo
                          , renderChar
                          , renderChars
                          , renderStr
                          , renderTxt
                          , preferredBuffering
                          , setRenderSize
                          -- reexports from Render.Backends.Internal.Delta
                          , Color8Code(..)
                          , setColor
                          , setColors
                          , Colors(..)
                          ) where

import           Imajuscule.Prelude

import           Control.Monad( void )

import           Data.Text( Text )
import           Data.String( String )

import           System.IO( BufferMode(..) )

import           Geo.Discrete.Types

import           Render.Backends.Internal.Delta
import           Render.Backends.Internal.Types

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering Nothing

beginFrame :: IO ()
beginFrame = return ()

endFrame :: IO ()
endFrame = swapAndFlush True {- clear buffer after rendering -}

moveTo :: Coords -> IO ()
moveTo (Coords (Row r) (Col c))
  | r < 0 || c < 0 = error "cannot render to negative locations"
  | otherwise = setDrawingPosition (fromIntegral c) (fromIntegral r)

renderChar :: Char -> IO ()
renderChar c = putCharRaw c

renderChars :: Int -> Char -> IO ()
renderChars n
  | n < 0 = error "cannot render a negative number of chars"
  | otherwise = putChars (fromIntegral n)

renderStr :: String -> IO ()
renderStr = putStr

renderTxt :: Text -> IO ()
renderTxt = putText

restoreColors :: Colors -> IO ()
restoreColors = void . setColors
