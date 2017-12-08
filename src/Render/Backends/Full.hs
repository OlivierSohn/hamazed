{-# LANGUAGE NoImplicitPrelude #-}

module Render.Backends.Full( beginFrame
                           , endFrame
                           , setDrawColor
                           , moveTo
                           , drawChar
                           , drawChars
                           , drawStr
                           , drawTxt
                           , setDrawColors
                           , restoreDrawColors
                           , preferredBuffering
                           , setRenderSize
                           -- reexport from Render.Backends.Internal.Types
                           , Colors(..)
                           ) where

import           Imajuscule.Prelude

import qualified Prelude( putChar
                        , putStr )

import           Control.Monad( replicateM_ )
import           Data.String( String )
import           Data.Text( Text, unpack )

import           System.Console.ANSI( Color8Code(..)
                                    , clearScreen
                                    , setCursorPosition
                                    , setSGR
                                    , SGR(..)
                                    , ConsoleLayer(..) )
import           System.IO( hFlush
                          , stdout
                          , BufferMode(..) )

import           Geo.Discrete.Types( Coords(..), Col(..), Row(..))

import           Render.Backends.Internal.Types

setRenderSize :: Int -> Int -> IO ()
setRenderSize _ _ = return ()

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering Nothing

beginFrame :: IO ()
beginFrame = clearScreen

endFrame :: IO ()
endFrame = hFlush stdout

moveTo :: Coords -> IO ()
moveTo (Coords (Row r) (Col c)) =
  setCursorPosition r c

drawChar :: Char -> IO ()
drawChar = Prelude.putChar

drawChars :: Int -> Char -> IO ()
drawChars n c = replicateM_ n $ Prelude.putChar c

drawStr :: String -> IO ()
drawStr = Prelude.putStr

drawTxt :: Text -> IO ()
drawTxt = Prelude.putStr . unpack

-- | These values are used to return hardcoded values when we can't know
--   the current foreground or background color
whiteColor8Code, blackColor8Code :: Color8Code
whiteColor8Code = Color8Code 231
blackColor8Code = Color8Code 16

-- | limited support : the returned value is hardcoded to white because there is no way
--   of getting the current color using System.Console.ANSI. TODO use a state monad
setDrawColor :: ConsoleLayer -> Color8Code -> IO Colors
setDrawColor layer c =
  setSGR [SetPaletteColor layer c] >>
    return (Colors whiteColor8Code blackColor8Code)

-- | limited support : the returned value is hardcoded to white forgroundm black background
--  because there is no way of getting the current color using System.Console.ANSI. TODO use a state monad
setDrawColors :: Colors -> IO Colors
setDrawColors (Colors fg bg) =
  setSGR [SetPaletteColor Foreground fg, SetPaletteColor Background bg] >>
    return (Colors whiteColor8Code blackColor8Code)

restoreDrawColors :: Colors -> IO ()
restoreDrawColors = void . setDrawColors
