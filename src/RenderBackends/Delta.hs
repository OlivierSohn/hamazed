{-# LANGUAGE NoImplicitPrelude #-}

module RenderBackends.Delta(
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

import           Control.Monad( void )

import           Data.Text( Text )
import           Data.String( String )

import           System.IO( hFlush
                          , stdout
                          , BufferMode(..) )

import           RenderBackends.Internal.Delta( blitBuffer
                                              , bSetForeground
                                              , bGotoXY
                                              , bPutCharRaw
                                              , bPutStr
                                              , bPutText
                                              , Color(..)
                                              , ColorIntensity(..) )

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

setForeground :: ColorIntensity -> Color -> IO (ColorIntensity, Color)
setForeground a b = bSetForeground (a,b)

restoreForeground :: (ColorIntensity, Color) -> IO ()
restoreForeground c = void $ bSetForeground c
