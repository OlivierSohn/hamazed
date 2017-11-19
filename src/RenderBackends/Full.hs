{-# LANGUAGE NoImplicitPrelude #-}

module RenderBackends.Full(
                            beginFrame
                          , endFrame
                          , setForeground
                          , moveTo
                          , renderChar
                          , renderStr
                          , renderTxt
                          , preferredBuffering
                          ) where

import           Imajuscule.Prelude

import qualified Prelude( putChar
                        , putStr )

import           Data.String( String )
import           Data.Text( Text, unpack )

import           System.Console.ANSI( Color(..)
                                    , ColorIntensity(..)
                                    , clearScreen
                                    , setCursorPosition
                                    , setSGR
                                    , SGR(..)
                                    , ConsoleLayer(..) )
import           System.IO( hFlush
                          , stdout
                          , BufferMode(..) )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..))


preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering Nothing

beginFrame :: IO ()
beginFrame = clearScreen

endFrame :: IO ()
endFrame = hFlush stdout

moveTo :: Coords -> IO ()
moveTo (Coords (Row r) (Col c)) =
  setCursorPosition r c

renderChar :: Char -> IO ()
renderChar = Prelude.putChar

renderStr :: String -> IO ()
renderStr = Prelude.putStr

renderTxt :: Text -> IO ()
renderTxt = Prelude.putStr . unpack

setForeground :: ColorIntensity -> Color -> IO ()
setForeground ci c = setSGR [SetColor Foreground ci c]
