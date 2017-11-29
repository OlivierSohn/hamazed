{-# LANGUAGE NoImplicitPrelude #-}

module RenderBackends.Full(
                            beginFrame
                          , endFrame
                          , setForeground
                          , restoreForeground
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

import           Geo.Types( Coords(..), Col(..), Row(..))


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

-- | limited support : the returned value is hardcoded because there is no way
--   of getting the current color using System.Console.ANSI. TODO use a state monad
setForeground :: ColorIntensity -> Color -> IO (ColorIntensity, Color)
setForeground ci c =
  setSGR [SetColor Foreground ci c] >>
    return (Vivid, White)

restoreForeground :: (ColorIntensity, Color) -> IO ()
restoreForeground = void . uncurry setForeground
