{-# LANGUAGE NoImplicitPrelude #-}

module Render.Console ( ConsoleConfig(..)
               , configureConsoleFor
               -- rendering functions
               , beginFrame
               , endFrame
               , setColors
               , restoreColors
               , setForeground
               , setRawForeground
               , Backend.restoreForeground
               , renderChar_
               , renderChars
               , renderStr
               , renderStr_
               , renderTxt
               , renderTxt_
               , RenderState(..)
               , renderSegment
               -- reexport System.Console.ANSI
               , ColorIntensity(..)
               , Color(..)
               , Color8Code(..)
               , Xterm256Color(..)
               -- reexport System.Console.ANSI.Codes
               , xterm256ColorToCode
               ) where

import           Imajuscule.Prelude

import           Data.Maybe( fromMaybe )
import           Data.String( String )
import           Data.Text( Text )

import qualified System.Console.Terminal.Size as Terminal( size
                                                         , Window(..))

import           System.Console.ANSI( clearScreen
                                    , hideCursor
                                    , setSGR
                                    , setCursorPosition
                                    , showCursor
                                    , ColorIntensity(..)
                                    , Color(..)
                                    , Color8Code(..)
                                    , Xterm256Color(..) )
import           System.Console.ANSI.Codes( xterm256ColorToCode )
import           System.IO( hSetBuffering
                          , hGetBuffering
                          , hSetEcho
                          , BufferMode( .. )
                          , stdin
                          , stdout )


import           Geo.Discrete.Types
import           Geo.Discrete

import           Interpolation

{--
import qualified RenderBackends.Full as Backend( --}
--{--
import qualified Render.Backends.Delta as Backend( --}
                                                  beginFrame
                                                , endFrame
                                                , moveTo
                                                , renderChar
                                                , renderChars
                                                , renderStr
                                                , renderTxt
                                                , setForeground
                                                , setRawForeground
                                                , restoreForeground
                                                , setColors
                                                , restoreColors
                                                , preferredBuffering )

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data ConsoleConfig = Gaming | Editing

newtype RenderState = RenderState {
    _currentUpperLeftCorner :: Coords
} deriving(Eq, Ord, Show)

instance DiscretelyInterpolable RenderState where
  distance (RenderState from) (RenderState to) =
    distance from to
  interpolate (RenderState from) (RenderState to) progress =
    RenderState $ interpolate from to progress

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

configureConsoleFor :: ConsoleConfig -> IO ()
configureConsoleFor config = do
  hSetEcho stdin $ case config of
      Gaming  -> False
      Editing -> True
  case config of
    Gaming  -> do
      hideCursor
      clearScreen
    Editing -> do
      showCursor
      -- do not clearScreen, to retain a potential printed exception
      setSGR []
      maySz <- Terminal.size
      let (Terminal.Window x _) = fromMaybe (Terminal.Window 0 0) maySz
      setCursorPosition x 0
  let requiredOutputBuffering = case config of
        Gaming  -> Backend.preferredBuffering
        Editing -> LineBuffering
  hSetBuffering stdout requiredOutputBuffering

  let requiredInputBuffering = NoBuffering
  initialIb <- hGetBuffering stdin
  hSetBuffering stdin requiredInputBuffering
  ib <- hGetBuffering stdin
  when (ib /= requiredInputBuffering) $
      error $ "input buffering mode "
            ++ show initialIb
            ++ " could not be changed to "
            ++ show requiredInputBuffering
            ++ " instead it is now "
            ++ show ib

beginFrame :: IO ()
beginFrame = Backend.beginFrame

endFrame :: IO ()
endFrame = Backend.endFrame

setForeground :: ColorIntensity -> Color -> IO Color8Code
setForeground = Backend.setForeground

setRawForeground :: Color8Code -> IO Color8Code
setRawForeground = Backend.setRawForeground

setColors :: (Color8Code, Color8Code) -> IO (Color8Code, Color8Code)
setColors = Backend.setColors

restoreColors :: (Color8Code, Color8Code) -> IO ()
restoreColors = Backend.restoreColors

renderChar_ :: Char -> RenderState -> IO ()
renderChar_ char (RenderState c) = do
  Backend.moveTo c
  Backend.renderChar char


renderChars :: Int -> Char -> RenderState -> IO ()
renderChars count char (RenderState c) = do
  Backend.moveTo c
  Backend.renderChars count char


renderStr :: String -> RenderState -> IO RenderState
renderStr str r@(RenderState c) =
  renderStr_ str r >> return (RenderState $ translateInDir Down c)

renderStr_ :: String -> RenderState -> IO ()
renderStr_ str (RenderState c) = do
  Backend.moveTo c
  Backend.renderStr str

renderTxt :: Text -> RenderState -> IO RenderState
renderTxt txt r@(RenderState c) =
  renderTxt_ txt r >> return (RenderState $ translateInDir Down c)

renderTxt_ :: Text -> RenderState -> IO ()
renderTxt_ txt (RenderState c) = do
  Backend.moveTo c
  Backend.renderTxt txt

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
