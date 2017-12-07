{-# LANGUAGE NoImplicitPrelude #-}

module Render.Backends.Full(
                            beginFrame
                          , endFrame
                          , setForeground
                          , setRawForeground
                          , restoreForeground
                          , moveTo
                          , renderChar
                          , renderChars
                          , renderStr
                          , renderTxt
                          , setColors
                          , restoreColors
                          , preferredBuffering
                          ) where

import           Imajuscule.Prelude

import qualified Prelude( putChar
                        , putStr )

import           Control.Monad( replicateM_ )
import           Data.String( String )
import           Data.Text( Text, unpack )

import           System.Console.ANSI( Color(..)
                                    , ColorIntensity(..)
                                    , Color8Code(..)
                                    , clearScreen
                                    , setCursorPosition
                                    , setSGR
                                    , SGR(..)
                                    , ConsoleLayer(..) )
import           System.IO( hFlush
                          , stdout
                          , BufferMode(..) )

import           Geo.Discrete.Types( Coords(..), Col(..), Row(..))


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

renderChars :: Int -> Char -> IO ()
renderChars n c = replicateM_ n $ Prelude.putChar c

renderStr :: String -> IO ()
renderStr = Prelude.putStr

renderTxt :: Text -> IO ()
renderTxt = Prelude.putStr . unpack

-- | These values are used to return hardcoded values when we can't know
--   the current foreground or background color
whiteColor8Code, blackColor8Code :: Color8Code
whiteColor8Code = Color8Code 231
blackColor8Code = Color8Code 16

-- | limited support : the returned value is hardcoded because there is no way
--   of getting the current color using System.Console.ANSI. TODO use a state monad
setForeground :: ColorIntensity -> Color -> IO Color8Code
setForeground ci c =
  setSGR [SetColor Foreground ci c] >>
    return whiteColor8Code

restoreForeground :: Color8Code -> IO ()
restoreForeground = void . setRawForeground

-- | limited support : the returned value is hardcoded to white because there is no way
--   of getting the current color using System.Console.ANSI. TODO use a state monad
setRawForeground :: Color8Code -> IO Color8Code
setRawForeground c =
  setSGR [SetPaletteColor Foreground c] >>
    return whiteColor8Code

-- | limited support : the returned value is hardcoded to white forgroundm black background
--  because there is no way of getting the current color using System.Console.ANSI. TODO use a state monad
setColors :: (Color8Code, Color8Code) -> IO (Color8Code, Color8Code)
setColors (fg, bg) = setSGR [SetPaletteColor Foreground fg, SetPaletteColor Background bg] >>
  return (whiteColor8Code, blackColor8Code)

restoreColors :: (Color8Code, Color8Code) -> IO ()
restoreColors = void . setColors
