{-# LANGUAGE NoImplicitPrelude #-}

module Render.Console
               ( ConsoleConfig(..)
               , configureConsoleFor
               -- rendering functions
               , drawStr
               , drawStr_
               , drawTxt
               , drawTxt_
               -- reexport System.Console.ANSI
               , Color8Code(..)
               , ConsoleLayer(..)
               -- reexport System.Console.ANSI.Codes
               , xterm256ColorToCode
               -- reexports from backends
               , Backend.Buffers
               , Backend.newDefaultContext
               , Backend.newContext
               , Backend.setResizePolicy
               , Backend.setClearPolicy
               , Backend.drawChars
               , Backend.drawChar
               , Backend.flush
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


import           Color.Types
import           Geo.Discrete.Types
import           Geo.Discrete

import qualified Render.Backends.Delta as Backend

import           Render.Types

data ConsoleConfig = Gaming | Editing

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

drawStr :: String -> Coords -> LayeredColor -> IORef Backend.Buffers -> IO Coords
drawStr str pos color b =
  Backend.drawStr str pos color b >> return (translateInDir Down pos)

drawStr_ :: String -> Coords -> LayeredColor -> IORef Backend.Buffers -> IO ()
drawStr_ s c co b =
  void (Backend.drawStr s c co b)

drawTxt :: Text -> Coords -> LayeredColor -> IORef Backend.Buffers -> IO Coords
drawTxt txt pos color b =
  Backend.drawTxt txt pos color b >> return (translateInDir Down pos)

drawTxt_ :: Text -> Coords -> LayeredColor -> IORef Backend.Buffers -> IO ()
drawTxt_ t c b bu =
  void (Backend.drawTxt t c b bu)
