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
               , setFrameDimensions
               -- reexport System.Console.ANSI
               , Color8Code(..)
               , ConsoleLayer(..)
               -- reexport System.Console.ANSI.Codes
               , xterm256ColorToCode
               -- reexports from backends
               , Backend.beginFrame
               , Backend.endFrame
               , Backend.setDrawColors
               , Backend.setDrawColor
               , Backend.Colors(..)
               , Backend.Context
               , Backend.newContext
               -- reexports
               , module Render.Types
               ) where

import           Imajuscule.Prelude

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

data RenderState = RenderState {
    _currentUpperLeftCorner :: !Coords
  , _currentContext :: !Backend.Context
} deriving(Eq, Show)

instance DiscretelyInterpolable RenderState where
  distance (RenderState from _) (RenderState to _) =
    distance from to
  interpolate (RenderState from ctxt) (RenderState to _) progress =
    RenderState (interpolate from to progress) ctxt

setFrameDimensions :: RenderSize -> Backend.Context -> IO ()
setFrameDimensions (UserDefined w h) ctxt
  | w <= 0 = error "negative or zero render width not allowed"
  | h <= 0 = error "negative or zero render height not allowed"
  | otherwise = Backend.setFrameDimensions (fromIntegral w) (fromIntegral h) ctxt
setFrameDimensions TerminalSize ctxt = do
  mayTermSize <- Terminal.size
  let (width, height) =
        maybe
          (300, 70) -- sensible default if terminal size is not available
          (\(Terminal.Window h w) -> (w, h))
            mayTermSize
  Backend.setFrameDimensions width height ctxt

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
      maybe
        (return ())
        (\(Terminal.Window x _) -> setCursorPosition (pred x) 0)
          maySz
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
renderChar_ char (RenderState c ctxt) = do
  Backend.moveTo c ctxt
  Backend.drawChar char ctxt


drawChars :: Int -> Char -> RenderState -> IO ()
drawChars count char (RenderState c ctxt) = do
  Backend.moveTo c ctxt
  Backend.drawChars count char ctxt

drawStr :: String -> RenderState -> IO RenderState
drawStr str r@(RenderState c ctxt) =
  renderStr_ str r >> return (RenderState (translateInDir Down c) ctxt)

renderStr_ :: String -> RenderState -> IO ()
renderStr_ str (RenderState c ctxt) = do
  Backend.moveTo c ctxt
  Backend.drawStr str ctxt

drawTxt :: Text -> RenderState -> IO RenderState
drawTxt txt r@(RenderState c ctxt) =
  renderTxt_ txt r >> return (RenderState (translateInDir Down c) ctxt)

renderTxt_ :: Text -> RenderState -> IO ()
renderTxt_ txt (RenderState c ctxt) = do
  Backend.moveTo c ctxt
  Backend.drawTxt txt ctxt

renderSegment :: Segment -> Char -> RenderState -> IO ()
renderSegment l char rs = case l of
  Horizontal row c1 c2 -> renderHorizontalLine row c1 c2 char rs
  Vertical col r1 r2   -> renderVerticalLine   col r1 r2 char rs
  Oblique _ _ -> error "oblique segment rendering is not supported"


renderVerticalLine :: Col -> Int -> Int -> Char -> RenderState -> IO ()
renderVerticalLine col r1 r2 char (RenderState upperLeft ctxt) = do
  let rows = [(min r1 r2)..(max r1 r2)]
  mapM_ (\r -> let rs = RenderState (sumCoords upperLeft $ Coords (Row r) col) ctxt
               in renderChar_ char rs) rows

renderHorizontalLine :: Row -> Int -> Int -> Char -> RenderState -> IO ()
renderHorizontalLine row c1 c2 char (RenderState upperLeft ctxt) = do
  let rs = RenderState (sumCoords upperLeft $ Coords row (Col (min c1 c2))) ctxt
  renderStr_ (replicate (1 + abs (c2-c1)) char) rs
