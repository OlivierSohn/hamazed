{-# LANGUAGE NoImplicitPrelude #-}

module Console ( ConsoleConfig(..)
               , configureConsoleFor
               -- rendering functions
               , beginFrame
               , endFrame
               , setForeground
               , renderChar_
               , renderStr
               , renderStr_
               , renderText_
               , RenderState(..)
               , renderSegment
               -- reexport System.Console.ANSI
               , ColorIntensity(..)
               , Color(..)
               ) where

import           Imajuscule.Prelude

import           Data.String( String )
import           Data.Text( Text )

import           System.Console.ANSI( clearScreen
                                    , hideCursor
                                    , setSGR
                                    , showCursor
                                    , SGR(..)
                                    , ConsoleLayer(..)
                                    , ColorIntensity(..)
                                    , Color(..) )
import           System.IO( hSetBuffering
                          , hSetEcho
                          , BufferMode( .. )
                          , stdin
                          , stdout )


import           Geo( Col(..)
                    , Coords(..)
                    , Segment(..)
                    , sumCoords
                    , translateInDir
                    , Direction(..)
                    , Row(..) )

{--
import qualified RenderBackends.Full as Backend( --}
--{--
import qualified RenderBackends.Delta as Backend( --}
                                                  beginFrame
                                                , endFrame
                                                , moveTo
                                                , renderChar
                                                , renderStr
                                                , renderTxt
                                                , setForeground
                                                , preferredBuffering )

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data ConsoleConfig = Gaming | Editing

newtype RenderState = RenderState {
    _currentUpperLeftCorner :: Coords
}

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

configureConsoleFor :: ConsoleConfig -> IO ()
configureConsoleFor config = do
  hSetEcho stdin $ case config of
      Gaming  -> False
      Editing -> True
  hSetBuffering stdout $ case config of
      Gaming  ->
        Backend.preferredBuffering
      Editing -> LineBuffering
  case config of
    Gaming  -> do
      hideCursor
      clearScreen
    Editing -> do
      showCursor
      -- do not clearScreen, to retain a potential printed exception
      setSGR [SetColor Foreground Vivid White]

beginFrame :: IO ()
beginFrame = Backend.beginFrame

endFrame :: IO ()
endFrame = Backend.endFrame

setForeground :: ColorIntensity -> Color -> IO ()
setForeground = Backend.setForeground

renderChar_ :: Char -> RenderState -> IO ()
renderChar_ char (RenderState c) = do
  Backend.moveTo c
  Backend.renderChar char
  return ()


renderStr_ :: String -> RenderState -> IO ()
renderStr_ str (RenderState c) = do
  Backend.moveTo c
  Backend.renderStr str

renderText_ :: Text -> RenderState -> IO ()
renderText_ txt (RenderState c) = do
  Backend.moveTo c
  Backend.renderTxt txt

renderStr :: String -> RenderState -> IO RenderState
renderStr str (RenderState c) = do
  Backend.moveTo c
  Backend.renderStr str
  return $ RenderState $ translateInDir Down c


renderSegment :: Segment -> Char -> RenderState -> IO ()
renderSegment l = case l of
  Horizontal row c1 c2 -> renderHorizontalLine row c1 c2
  Vertical col r1 r2   -> renderVerticalLine   col r1 r2
  Oblique _ _ -> error "oblique segment rendering is not supported"


renderVerticalLine :: Col -> Int -> Int -> Char -> RenderState -> IO ()
renderVerticalLine col r1 r2 char (RenderState upperLeft) = do
  let rows = [(min r1 r2)..(max r1 r2)]
  mapM_ (renderChar_ char . (\r -> RenderState $ sumCoords upperLeft $ Coords (Row r) col)) rows

renderHorizontalLine :: Row -> Int -> Int -> Char -> RenderState -> IO ()
renderHorizontalLine row c1 c2 char (RenderState upperLeft) =
  renderStr_ (replicate (1 + abs (c2-c1)) char) $ RenderState $ sumCoords upperLeft $ Coords row (Col (min c1 c2))
