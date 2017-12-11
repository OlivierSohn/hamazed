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
               , Backend.Colors(..)
               , Backend.newContext
               , Backend.IORef
               , Backend.Buffers
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

{--
import qualified Render.Backends.Full as Backend --}
--{--
import qualified Render.Backends.Delta as Backend --}

import           Render.Types

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data ConsoleConfig = Gaming | Editing

setFrameDimensions :: RenderSize -> Backend.IORef Backend.Buffers -> IO ()
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

renderChar_ :: Char -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO ()
renderChar_ =
  Backend.drawChar


drawChars :: Int -> Char -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO ()
drawChars=
  Backend.drawChars

drawStr :: String -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO Coords
drawStr str color pos b =
  renderStr_ str color pos b >> return (translateInDir Down pos)

renderStr_ :: String -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO ()
renderStr_ =
  Backend.drawStr

drawTxt :: Text -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO Coords
drawTxt txt color pos b =
  renderTxt_ txt color pos b >> return (translateInDir Down pos)

renderTxt_ :: Text -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO ()
renderTxt_ =
  Backend.drawTxt

renderSegment :: Segment -> Char -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO ()
renderSegment l char rs = case l of
  Horizontal row c1 c2 -> renderHorizontalLine row c1 c2 char rs
  Vertical col r1 r2   -> renderVerticalLine   col r1 r2 char rs
  Oblique _ _ -> error "oblique segment rendering is not supported"


renderVerticalLine :: Col -> Int -> Int -> Char -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO ()
renderVerticalLine col r1 r2 char color pos b = do
  let rows = [(min r1 r2)..(max r1 r2)]
  mapM_ (\r -> let pos' = sumCoords pos $ Coords (Row r) col
               in renderChar_ char color pos' b) rows

renderHorizontalLine :: Row -> Int -> Int -> Char -> Backend.Colors -> Coords -> Backend.IORef Backend.Buffers -> IO ()
renderHorizontalLine row c1 c2 char color pos = do
  let pos' = sumCoords pos $ Coords row (Col (min c1 c2))
  renderStr_ (replicate (1 + abs (c2-c1)) char) color pos'
