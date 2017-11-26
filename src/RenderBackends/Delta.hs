{-# LANGUAGE NoImplicitPrelude #-}

module RenderBackends.Delta(
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
                          , Color8Code(..)
                          ) where

import           Imajuscule.Prelude

import           Control.Monad( void )

import           Data.Text( Text )
import           Data.String( String )

import           System.IO( hFlush
                          , stdout
                          , BufferMode(..) )

import           RenderBackends.Internal.Delta( blitBuffer
                                              , bSetForeground
                                              , bSetRawForeground
                                              , bSetColors
                                              , bGotoXY
                                              , bPutCharRaw
                                              , bPutCharsRaw
                                              , bPutStr
                                              , bPutText
                                              , Color(..)
                                              , ColorIntensity(..)
                                              , Color8Code(..) )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..))

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering Nothing

beginFrame :: IO ()
beginFrame = return ()

endFrame :: IO ()
endFrame = blitBuffer True {- clear buffer -} >> hFlush stdout

moveTo :: Coords -> IO ()
moveTo (Coords (Row r) (Col c)) = bGotoXY c r

renderChar :: Char -> IO ()
renderChar c = void (bPutCharRaw c)

renderChars :: Int -> Char -> IO ()
renderChars = bPutCharsRaw

renderStr :: String -> IO ()
renderStr = bPutStr

renderTxt :: Text -> IO ()
renderTxt = bPutText

setForeground :: ColorIntensity -> Color -> IO Color8Code
setForeground a b = bSetForeground (a,b)

setRawForeground :: Color8Code -> IO Color8Code
setRawForeground = bSetRawForeground

restoreForeground :: Color8Code -> IO ()
restoreForeground = void . setRawForeground

setColors :: (Color8Code, Color8Code) -> IO (Color8Code, Color8Code)
setColors = bSetColors

restoreColors :: (Color8Code, Color8Code) -> IO ()
restoreColors = void . bSetColors
