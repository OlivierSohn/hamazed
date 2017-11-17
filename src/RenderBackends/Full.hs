
module RenderBackends.Full(
                            beginFrame
                          , endFrame
                          , setForeground
                          , moveTo
                          , renderChar
                          , renderStr
                          ) where

import           Imajuscule.Prelude

import qualified Prelude( putChar
                        , putStr )

import           System.Console.ANSI( Color(..)
                                    , ColorIntensity(..)
                                    , clearScreen
                                    , setCursorPosition
                                    , setSGR
                                    , SGR(..)
                                    , ConsoleLayer(..) )
import           System.IO( hFlush
                          , stdout )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..))


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

setForeground :: ColorIntensity -> Color -> IO ()
setForeground ci c = setSGR [SetColor Foreground ci c]
