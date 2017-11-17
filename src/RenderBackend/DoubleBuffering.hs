module RenderBackend.DoubleBuffering(
                                      beginFrame
                                    , endFrame
                                    , setForeground
                                    , moveTo
                                    , renderChar
                                    , renderStr
                                    ) where

import           Imajuscule.Prelude

import           System.IO( hFlush
                          , stdout )

import           IncrementalRender( blitBuffer
                                  , bSetForeground
                                  , bGotoXY
                                  , bPutChar
                                  , bPutStr
                                  , Color(..)
                                  , ColorIntensity(..) )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..))
beginFrame :: IO ()
beginFrame = return ()

endFrame :: IO ()
endFrame = blitBuffer >> hFlush stdout

moveTo :: Coords -> IO ()
moveTo (Coords (Row r) (Col c)) = bGotoXY c r

renderChar :: Char -> IO ()
renderChar = bPutChar

renderStr :: String -> IO ()
renderStr = bPutStr

setForeground :: ColorIntensity -> Color -> IO ()
setForeground a b = bSetForeground (a,b)
