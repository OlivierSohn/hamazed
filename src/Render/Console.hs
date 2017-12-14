{-# LANGUAGE NoImplicitPrelude #-}

module Render.Console
               ( ConsoleConfig(..)
               , configureConsoleFor
               -- reexport System.Console.ANSI
               , Color8Code(..)
               , ConsoleLayer(..)
               -- reexport System.Console.ANSI.Codes
               , xterm256ColorToCode
               -- reexports
               , module Render.Types
               , module Render.Backends.Delta
               ) where

import           Imajuscule.Prelude

import           Data.String( String )

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

import           Render.Backends.Delta

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
      -- do not clearFromCursorToScreenEnd, to retain a potential printed exception
      setSGR []
      maySz <- Terminal.size
      maybe
        (return ())
        (\(Terminal.Window x _) -> setCursorPosition (pred x) 0)
          maySz
  let requiredOutputBuffering = case config of
        Gaming  -> preferredBuffering
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
