module RenderBackend.DoubleBuffering(
                                      beginFrame
                                    , endFrame
                                    , setForeground
                                    , moveTo
                                    , renderChar
                                    , renderStr
                                    ) where

import           Imajuscule.Prelude

import           Control.Monad( void )

import           System.IO( hFlush
                          , stdout )

import           IncrementalRender( blitBuffer, bClear
                                  , bSetForeground
                                  , bGotoXY
                                  , bPutCharRaw
                                  , bPutStr
                                  , Color(..)
                                  , ColorIntensity(..) )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..))
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

setForeground :: ColorIntensity -> Color -> IO ()
setForeground a b = bSetForeground (a,b)
