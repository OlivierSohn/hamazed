{-# LANGUAGE NoImplicitPrelude #-}

module RenderBackends.Delta(
                            beginFrame
                          , endFrame
                          , setForeground
                          , setRawForeground
                          , restoreForeground
                          , moveTo
                          , renderChar
                          , renderStr
                          , renderTxt
                          , preferredBuffering
                          , Raw8Color(..)
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
                                              , bGotoXY
                                              , bPutCharRaw
                                              , bPutStr
                                              , bPutText
                                              , Color(..)
                                              , ColorIntensity(..)
                                              , Raw8Color(..) )

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

renderStr :: String -> IO ()
renderStr = bPutStr

renderTxt :: Text -> IO ()
renderTxt = bPutText

setForeground :: ColorIntensity -> Color -> IO Raw8Color
setForeground a b = bSetForeground (a,b)

setRawForeground :: Raw8Color -> IO Raw8Color
setRawForeground = bSetRawForeground

restoreForeground :: Raw8Color -> IO ()
restoreForeground = void . setRawForeground
