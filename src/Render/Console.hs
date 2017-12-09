{-# LANGUAGE NoImplicitPrelude #-}

module Render.Console
               ( ConsoleConfig(..)
               , configureConsoleFor
               -- rendering functions
               , renderChar_
               , drawChars
               , drawStr
               , renderStr_
               , drawTxt
               , renderTxt_
               , RenderState(..)
               , renderSegment
               , setCanvasDimensions
               -- reexport System.Console.ANSI
               , Color8Code(..)
               , ConsoleLayer(..)
               -- reexport System.Console.ANSI.Codes
               , xterm256ColorToCode
               -- reexports from backends
               , Backend.beginFrame
               , Backend.endFrame
               , Backend.setDrawColors
               , Backend.restoreDrawColors
               , Backend.setDrawColor
               , Backend.Colors(..)
               -- reexports
               , module Render.Types
               ) where

import           Imajuscule.Prelude

import           Data.Maybe( fromMaybe )
import           Data.String( String )
import           Data.Text( Text )

import qualified System.Console.Terminal.Size as Terminal( size
                                                         , Window(..))

import           System.Console.ANSI( clearScreen, hideCursor
                                    , setSGR, setCursorPosition, showCursor
                                    , Color8Code(..), ConsoleLayer(..) )
import           System.Console.ANSI.Color( xterm256ColorToCode )
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
import qualified Render.Backends.Full as Backend --}
--{--
import qualified Render.Backends.Delta as Backend --}

import           Render.Types

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

setCanvasDimensions :: RenderSize -> IO ()
setCanvasDimensions (UserDefined w h)
  | w <= 0 = error "negative or zero render width not allowed"
  | h <= 0 = error "negative or zero render height not allowed"
  | otherwise = Backend.setCanvasDimensions (fromIntegral w) (fromIntegral h)
setCanvasDimensions TerminalSize = do
  mayTermSize <- Terminal.size
  let (width, height) =
        maybe
          (300, 70) -- sensible default if terminal size is not available
          (\(Terminal.Window h w) -> (w, h))
            mayTermSize
  Backend.setCanvasDimensions width height

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

renderChar_ :: Char -> RenderState -> IO ()
renderChar_ char (RenderState c) = do
  Backend.moveTo c
  Backend.drawChar char


drawChars :: Int -> Char -> RenderState -> IO ()
drawChars count char (RenderState c) = do
  Backend.moveTo c
  Backend.drawChars count char

drawStr :: String -> RenderState -> IO RenderState
drawStr str r@(RenderState c) =
  renderStr_ str r >> return (RenderState $ translateInDir Down c)

renderStr_ :: String -> RenderState -> IO ()
renderStr_ str (RenderState c) = do
  Backend.moveTo c
  Backend.drawStr str

drawTxt :: Text -> RenderState -> IO RenderState
drawTxt txt r@(RenderState c) =
  renderTxt_ txt r >> return (RenderState $ translateInDir Down c)

renderTxt_ :: Text -> RenderState -> IO ()
renderTxt_ txt (RenderState c) = do
  Backend.moveTo c
  Backend.drawTxt txt

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
